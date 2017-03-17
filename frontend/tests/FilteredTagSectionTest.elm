module FilteredTagSectionTest exposing (..)

import FilteredTag exposing (FilteredTag, Msg(..))
import FilteredTagSection exposing (..)
import Fuzz exposing (Fuzzer)
import FilteredTagTest
import Json.Decode exposing (decodeString)
import Tag exposing (..)
import Expect
import Test exposing (Test, describe, fuzz, test)


modelFuzzer : Fuzzer (FilteredTagSection Topic)
modelFuzzer =
    Fuzz.map2 FilteredTagSection
        Fuzz.string
        (Fuzz.list FilteredTagTest.modelFuzzer)


modelPartsForUpdateFuzzer : Fuzzer ( String, List (FilteredTag Topic), FilteredTag Topic, List (FilteredTag Topic) )
modelPartsForUpdateFuzzer =
    Fuzz.map4 (,,,)
        Fuzz.string
        (Fuzz.list FilteredTagTest.modelFuzzer)
        FilteredTagTest.modelFuzzer
        (Fuzz.list FilteredTagTest.modelFuzzer)
        |> Fuzz.map
            (\( s, t1, t, t2 ) ->
                ( s
                , List.filter (\t2 -> (t2.tag /= t.tag)) t1
                , t
                , List.filter (\t2 -> (t2.tag /= t.tag)) t2
                )
            )


modelFromParts : ( String, List (FilteredTag Topic), FilteredTag Topic, List (FilteredTag Topic) ) -> FilteredTagSection Topic
modelFromParts ( string, tags1, tag, tags2 ) =
    { sectionName = string, tags = List.concat [ tags1, [ tag ], tags2 ] }


tests =
    describe "FilteredTagSection"
        [ fuzz modelFuzzer "includedTags is the list of included tags" <|
            \model ->
                includedTags model
                    |> Expect.equal
                        (List.filterMap
                            (\ft ->
                                if ft.included then
                                    Just ft.tag
                                else
                                    Nothing
                            )
                            model.tags
                        )
        , fuzz modelPartsForUpdateFuzzer "update with UpdateTag Include updates the tags with the given tag" <|
            \( s, t1, t, t2 ) ->
                modelFromParts ( s, t1, t, t2 )
                    |> update (UpdateTag t.tag Include)
                    |> Expect.equal (modelFromParts ( s, t1, { t | included = True }, t2 ))
        , fuzz modelPartsForUpdateFuzzer "update with UpdateTag Exclude updates the tags with the given tag" <|
            \( s, t1, t, t2 ) ->
                modelFromParts ( s, t1, t, t2 )
                    |> update (UpdateTag t.tag Exclude)
                    |> Expect.equal (modelFromParts ( s, t1, { t | included = False }, t2 ))
        , let
            includedTags =
                [ Topic "Ruby", Topic "JavaScript" ]

            model =
                { sectionName = "section"
                , tags =
                    [ FilteredTag.init (Topic "Ruby")
                    , FilteredTag.init (Topic "England")
                    , FilteredTag.init (Topic "JavaScript")
                    , FilteredTag.init (Topic "USA")
                    ]
                }

            expectedModel =
                { sectionName = "section"
                , tags =
                    [ FilteredTag.init (Topic "Ruby") |> FilteredTag.update Include
                    , FilteredTag.init (Topic "England")
                    , FilteredTag.init (Topic "JavaScript") |> FilteredTag.update Include
                    , FilteredTag.init (Topic "USA")
                    ]
                }
          in
            test "initializeIncludedTags includes tags in the list" <|
                \() ->
                    (initializeIncludedTags model includedTags)
                        |> Expect.equal expectedModel
        , test "decodes from JSON" <|
            \() ->
                decodeString (decoder "section" Topic) "[\"Elm\", \"Ruby\"]"
                    |> Expect.equal
                        (Ok
                            { sectionName = "section"
                            , tags =
                                [ FilteredTag (Topic "Elm") False
                                , FilteredTag (Topic "Ruby") False
                                ]
                            }
                        )
        ]
