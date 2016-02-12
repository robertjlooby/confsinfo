module ConferenceTest (..) where

import Check.Test as Check
import Conference exposing (..)
import Date exposing (Month(..))
import DateFormatter exposing (compare')
import ElmTest exposing (assertEqual, suite, test)
import FilteredTag exposing (FilteredTag(..))
import Random
import Random.Extra
import Shrink exposing (Shrinker)
import Tag exposing (Tag(..))
import TestHelpers exposing (..)


blankConf : Conference
blankConf =
  { name = ""
  , link = ""
  , startDate = ( 1, Jan, 1 )
  , endDate = ( 1, Jan, 1 )
  , location = ""
  , cfpStartDate = Nothing
  , cfpEndDate = Nothing
  , tags = []
  }


tests =
  suite
    "Model"
    [ checkTest
        "cfpStatus is (Closed, Nothing) if start and end are Nothing"
        (\date -> cfpStatus date blankConf)
        (\_ -> ( Closed, Nothing ))
        { generator = randomDaTupleGenerator
        , shrinker = daTupleShrinker
        }
    , checkTest
        "cfpStatus is False if cfpEndDate is Nothing"
        (\date -> cfpStatus date { blankConf | cfpStartDate = Just date })
        (\_ -> ( Closed, Nothing ))
        { generator = randomDaTupleGenerator
        , shrinker = daTupleShrinker
        }
    , checkTest
        "cfpStatus is (Open, Just cfpEndDate) if cfpEndDate == currentDate"
        (\date -> cfpStatus date { blankConf | cfpEndDate = Just date })
        (\date -> ( Open, Just date ))
        { generator = randomDaTupleGenerator
        , shrinker = daTupleShrinker
        }
    , checkTest
        "cfpStatus is (Closed, Nothing) if currentDate is after cfpEndDate"
        (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate })
        (\_ -> ( Closed, Nothing ))
        { generator =
            Random.map2 (,) randomDaTupleGenerator randomDaTupleGenerator
              |> Random.Extra.keepIf (\( currentDate, endDate ) -> GT == compare' currentDate endDate)
        , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
        }
    , checkTest
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and cfpStartDate is Nothing"
        (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate })
        (\( _, date ) -> ( Open, Just date ))
        { generator =
            Random.map2 (,) randomDaTupleGenerator randomDaTupleGenerator
              |> Random.Extra.keepIf (\( currentDate, endDate ) -> LT == compare' currentDate endDate)
        , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
        }
    , checkTest
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and after cfpStartDate"
        (\( currentDate, startDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just currentDate })
        (\( _, _, date ) -> ( Open, Just date ))
        { generator =
            Random.map3 (,,) randomDaTupleGenerator randomDaTupleGenerator randomDaTupleGenerator
              |> Random.Extra.keepIf (\( currentDate, _, endDate ) -> LT == compare' currentDate endDate)
              |> Random.Extra.keepIf (\( currentDate, startDate, _ ) -> GT == compare' currentDate startDate)
        , shrinker = Shrink.tuple3 ( daTupleShrinker, daTupleShrinker, daTupleShrinker )
        }
    , checkTest
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and equal to cfpStartDate"
        (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just currentDate })
        (\( _, date ) -> ( Open, Just date ))
        { generator =
            Random.map2 (,) randomDaTupleGenerator randomDaTupleGenerator
              |> Random.Extra.keepIf (\( currentDate, endDate ) -> LT == compare' currentDate endDate)
        , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
        }
    , checkTest
        "cfpStatus is (NotYetOpen, Just cfpStartDate) if currentDate is before cfpEndDate and cfpStartDate"
        (\( currentDate, startDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just startDate })
        (\( _, date, _ ) -> ( NotYetOpen, Just date ))
        { generator =
            Random.map3 (,,) randomDaTupleGenerator randomDaTupleGenerator randomDaTupleGenerator
              |> Random.Extra.keepIf (\( _, startDate, endDate ) -> LT == compare' startDate endDate)
              |> Random.Extra.keepIf (\( currentDate, startDate, _ ) -> LT == compare' currentDate startDate)
        , shrinker = Shrink.tuple3 ( daTupleShrinker, daTupleShrinker, daTupleShrinker )
        }
    ]
