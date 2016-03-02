module FilteredTagTest (..) where

import Check exposing (claim, for, is, suite, that)
import Check.Test
import FilteredTag exposing (..)
import Lazy.List exposing ((:::), empty)
import Random
import Random.Extra
import Shrink
import Tag exposing (Tag(FunctionalProgramming))
import TestHelpers exposing (allTags)


randomTagGenerator : Random.Generator Tag
randomTagGenerator =
  Random.Extra.selectWithDefault FunctionalProgramming allTags


randomFilterGenerator : Random.Generator (Tag -> FilteredTag)
randomFilterGenerator =
  Random.Extra.selectWithDefault Excluded [ Excluded, Included ]


tests =
  Check.Test.evidenceToTest <| Check.quickCheck claims


claims : Check.Claim
claims =
  suite
    "FilteredTag"
    [ claim
        "getTag gets the tag from a FilteredTag"
        `that` (\( filter, tag ) -> getTag (filter tag))
        `is` (\( _, tag ) -> tag)
        `for` { generator = Random.map2 (,) randomFilterGenerator randomTagGenerator
              , shrinker = Shrink.tuple ( Shrink.noShrink, Shrink.noShrink )
              }
    , claim
        "isIncluded returns True for an Included tag"
        `that` (\tag -> isIncluded (Included tag))
        `is` (\_ -> True)
        `for` { generator = randomTagGenerator
              , shrinker = Shrink.noShrink
              }
    , claim
        "isIncluded returns False for an Excluded tag"
        `that` (\tag -> isIncluded (Excluded tag))
        `is` (\_ -> False)
        `for` { generator = randomTagGenerator
              , shrinker = Shrink.noShrink
              }
    ]
