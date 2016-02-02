module FilteredTagTest (..) where

import Check.Test as Check
import ElmTest exposing (assertEqual, suite, test)
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
  suite
    "FilteredTag"
    [ Check.test
        "getTag gets the tag from a FilteredTag"
        (\( filter, tag ) -> getTag (filter tag))
        (\( _, tag ) -> tag)
        { generator = Random.map2 (,) randomFilterGenerator randomTagGenerator
        , shrinker = Shrink.tuple ( Shrink.noShrink, Shrink.noShrink )
        }
        100
        (Random.initialSeed 1)
    , Check.test
        "isIncluded returns True for an Included tag"
        (\tag -> isIncluded (Included tag))
        (\_ -> True)
        { generator = randomTagGenerator
        , shrinker = Shrink.noShrink
        }
        100
        (Random.initialSeed 1)
    , Check.test
        "isIncluded returns False for an Excluded tag"
        (\tag -> isIncluded (Excluded tag))
        (\_ -> False)
        { generator = randomTagGenerator
        , shrinker = Shrink.noShrink
        }
        100
        (Random.initialSeed 1)
    ]
