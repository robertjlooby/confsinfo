module FilteredTagSectionTest exposing (..)

import Check exposing (claim, for, is, that)
import Check.Test
import ElmTest exposing (Test, test, assertEqual)
import FilteredTag
import FilteredTagInternal exposing (..)
import FilteredTagSection exposing (..)
import FilteredTagSectionInternal exposing (..)
import FilteredTagTest
import Random
import Random.Extra
import Random.List
import Random.String
import Shrink
import Tag exposing (Tag(..))


randomModel : Random.Generator Model
randomModel =
  Random.map2
    (\tags string -> { tags = tags, sectionName = string })
    (Random.List.rangeLengthList 0 10 FilteredTagTest.randomModel)
    Random.String.anyEnglishWord


randomModelPartsForUpdate : Random.Generator ( String, List FilteredTag.Model, FilteredTag.Model, List FilteredTag.Model )
randomModelPartsForUpdate =
  Random.map4
    (,,,)
    Random.String.anyEnglishWord
    (Random.List.rangeLengthList 0 10 FilteredTagTest.randomModel)
    FilteredTagTest.randomModel
    (Random.List.rangeLengthList 0 10 FilteredTagTest.randomModel)
    |> Random.Extra.dropIf
        (\( _, t1, t, t2 ) ->
          List.concat [ t1, t2 ]
            |> List.map .tag
            |> List.member t.tag
        )


modelFromParts : ( String, List FilteredTag.Model, FilteredTag.Model, List FilteredTag.Model ) -> Model
modelFromParts ( string, tags1, tag, tags2 ) =
  { sectionName = string, tags = List.concat [ tags1, [ tag ], tags2 ] }


tests =
  ElmTest.suite
    "FilteredTagSection"
    [ Check.Test.evidenceToTest <| Check.quickCheck claims
    , unitTests
    ]


claims : Check.Claim
claims =
  Check.suite
    "check"
    [ claim
        "includedTags is the list of included tags"
        `that` (\model -> includedTags model)
        `is` (\{ tags } ->
                List.filterMap
                  (\ft ->
                    if ft.state == Included then
                      Just ft.tag
                    else
                      Nothing
                  )
                  tags
             )
        `for` { generator = randomModel
              , shrinker = Shrink.noShrink
              }
    , claim
        "update with UpdateTag Include updates the tags with the given tag"
        `that` (\( s, t1, t, t2 ) -> modelFromParts ( s, t1, t, t2 ) |> update (UpdateTag t.tag Include))
        `is` (\( s, t1, t, t2 ) -> modelFromParts ( s, t1, { t | state = Included }, t2 ))
        `for` { generator = randomModelPartsForUpdate
              , shrinker = Shrink.noShrink
              }
    , claim
        "update with UpdateTag Exclude updates the tags with the given tag"
        `that` (\( s, t1, t, t2 ) -> modelFromParts ( s, t1, t, t2 ) |> update (UpdateTag t.tag Exclude))
        `is` (\( s, t1, t, t2 ) -> modelFromParts ( s, t1, { t | state = Excluded }, t2 ))
        `for` { generator = randomModelPartsForUpdate
              , shrinker = Shrink.noShrink
              }
    , claim
        "update with Reset excludes all the tags"
        `that` (\model -> update Reset model)
        `is` (\model -> { model | tags = List.map (\t -> { t | state = Excluded }) model.tags })
        `for` { generator = randomModel
              , shrinker = Shrink.noShrink
              }
    ]


unitTests : Test
unitTests =
  ElmTest.suite
    "test"
    [ let
        includedTags =
          List.map toString [ Ruby, JavaScript ]

        model =
          { sectionName = "section"
          , tags =
              [ FilteredTag.init Ruby ""
              , FilteredTag.init England ""
              , FilteredTag.init JavaScript ""
              , FilteredTag.init USA ""
              ]
          }

        expectedModel =
          { sectionName = "section"
          , tags =
              [ FilteredTag.init Ruby "" |> FilteredTag.update Include
              , FilteredTag.init England ""
              , FilteredTag.init JavaScript "" |> FilteredTag.update Include
              , FilteredTag.init USA ""
              ]
          }
      in
        test
          "initializeIncludedTags includes tags in the list"
          <| assertEqual
              (initializeIncludedTags includedTags model)
              expectedModel
    ]
