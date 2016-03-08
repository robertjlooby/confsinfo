module FilteredTagTest (..) where

import Check exposing (claim, for, is, suite, that)
import Check.Test
import FilteredTag exposing (..)
import FilteredTagInternal exposing (..)
import Lazy.List exposing ((:::), empty)
import Random
import Random.Extra
import Random.List
import Random.String
import Shrink
import Tag exposing (Tag(FunctionalProgramming))
import TestHelpers exposing (..)


randomState : Random.Generator State
randomState =
  Random.Extra.selectWithDefault Excluded [ Included, Excluded ]


randomModel : Random.Generator Model
randomModel =
  Random.map3
    (\tag state string -> { tag = tag, state = state, display = string })
    randomTag
    randomState
    Random.String.anyEnglishWord


tests =
  Check.Test.evidenceToTest <| Check.quickCheck claims


claims : Check.Claim
claims =
  suite
    "FilteredTag"
    [ claim
        "init starts with the tag Excluded"
        `that` (\( tag, string ) -> init tag string)
        `is` (\( tag, string ) -> { tag = tag, state = Excluded, display = string })
        `for` { generator = Random.map2 (,) randomTag Random.String.anyEnglishWord
              , shrinker = Shrink.tuple ( Shrink.noShrink, Shrink.string )
              }
    , claim
        "update with Include includes a model"
        `that` (\model -> update Include model |> .state)
        `is` (\_ -> Included)
        `for` { generator = randomModel
              , shrinker = Shrink.noShrink
              }
    , claim
        "update with Exclude excludes a model"
        `that` (\model -> update Exclude model |> .state)
        `is` (\_ -> Excluded)
        `for` { generator = randomModel
              , shrinker = Shrink.noShrink
              }
    , claim
        "initializeIncludedTag includes a model if its tag is in the list"
        `that` (\( tags, model ) -> initializeIncludedTag tags model |> .state)
        `is` (\_ -> Included)
        `for` { generator =
                  Random.map2
                    (\tags model -> ( toString model.tag :: tags, model ))
                    randomListOfTagsStrings
                    randomModel
              , shrinker = Shrink.noShrink
              }
    , claim
        "initializeIncludedTag returns the model if its tag is not in the list"
        `that` (\( tags, model ) -> initializeIncludedTag tags model)
        `is` (\( _, model ) -> model)
        `for` { generator =
                  Random.map2
                    (\tags model -> ( List.filter (\t -> t /= toString model.tag) tags, model ))
                    randomListOfTagsStrings
                    randomModel
              , shrinker = Shrink.noShrink
              }
    , claim
        "exclude excludes a model"
        `that` (\model -> exclude model |> .state)
        `is` (\_ -> Excluded)
        `for` { generator = randomModel
              , shrinker = Shrink.noShrink
              }
    , claim
        "isIncluded returns True for an Included tag"
        `that` (\model -> isIncluded <| { model | state = Included })
        `is` (\_ -> True)
        `for` { generator = randomModel
              , shrinker = Shrink.noShrink
              }
    , claim
        "isIncluded returns False for an Excluded tag"
        `that` (\model -> isIncluded <| { model | state = Excluded })
        `is` (\_ -> False)
        `for` { generator = randomModel
              , shrinker = Shrink.noShrink
              }
    ]
