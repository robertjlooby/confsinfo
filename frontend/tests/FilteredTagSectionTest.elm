module FilteredTagSectionTest exposing (..)

import FilteredTag exposing (FilteredTag, Msg(..), State(..))
import FilteredTagSection exposing (..)
import Fuzz exposing (Fuzzer)
import FilteredTagTest
import Tag exposing (..)
import Expect
import Test exposing (Test, describe, fuzz, test)


modelFuzzer : Fuzzer (FilteredTagSection Tag)
modelFuzzer =
    Fuzz.map2 FilteredTagSection
        Fuzz.string
        (Fuzz.list FilteredTagTest.modelFuzzer)


modelPartsForUpdateFuzzer : Fuzzer ( String, List (FilteredTag Tag), FilteredTag Tag, List (FilteredTag Tag) )
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


modelFromParts : ( String, List (FilteredTag Tag), FilteredTag Tag, List (FilteredTag Tag) ) -> FilteredTagSection Tag
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
                                if ft.state == Included then
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
                    |> Expect.equal (modelFromParts ( s, t1, { t | state = Included }, t2 ))
        , fuzz modelPartsForUpdateFuzzer "update with UpdateTag Exclude updates the tags with the given tag" <|
            \( s, t1, t, t2 ) ->
                modelFromParts ( s, t1, t, t2 )
                    |> update (UpdateTag t.tag Exclude)
                    |> Expect.equal (modelFromParts ( s, t1, { t | state = Excluded }, t2 ))
        , fuzz modelFuzzer "update with Reset excludes all the tags" <|
            \model ->
                update Reset model
                    |> Expect.equal { model | tags = List.map (\t -> { t | state = Excluded }) model.tags }
        , let
            includedTags =
                [ Tag "Ruby", Tag "JavaScript" ]

            model =
                { sectionName = "section"
                , tags =
                    [ FilteredTag.init (Tag "Ruby")
                    , FilteredTag.init (Tag "England")
                    , FilteredTag.init (Tag "JavaScript")
                    , FilteredTag.init (Tag "USA")
                    ]
                }

            expectedModel =
                { sectionName = "section"
                , tags =
                    [ FilteredTag.init (Tag "Ruby") |> FilteredTag.update Include
                    , FilteredTag.init (Tag "England")
                    , FilteredTag.init (Tag "JavaScript") |> FilteredTag.update Include
                    , FilteredTag.init (Tag "USA")
                    ]
                }
          in
            test "initializeIncludedTags includes tags in the list" <|
                \() ->
                    (initializeIncludedTags includedTags model)
                        |> Expect.equal expectedModel
        ]
