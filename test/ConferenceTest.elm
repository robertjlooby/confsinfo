module ConferenceTest (..) where

import Check exposing (claim, for, is, suite, that)
import Check.Test
import Conference exposing (..)
import ConferenceInternal exposing (..)
import Date exposing (Month(..))
import DaTuple exposing (compare')
import Random
import Random.Extra
import Shrink exposing (Shrinker)
import Tag exposing (Tag(..))
import TestHelpers exposing (..)


blankConf : Conference.Model
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
  Check.Test.evidenceToTest <| Check.quickCheck claims


claims : Check.Claim
claims =
  suite
    "Model"
    [ claim
        "cfpStatus is (Closed, Nothing) if start and end are Nothing"
        `that` (\date -> cfpStatus date blankConf)
        `is` (\_ -> ( Closed, Nothing ))
        `for` { generator = randomDaTuple
              , shrinker = daTupleShrinker
              }
    , claim
        "cfpStatus is False if cfpEndDate is Nothing"
        `that` (\date -> cfpStatus date { blankConf | cfpStartDate = Just date })
        `is` (\_ -> ( Closed, Nothing ))
        `for` { generator = randomDaTuple
              , shrinker = daTupleShrinker
              }
    , claim
        "cfpStatus is (Open, Just cfpEndDate) if cfpEndDate == currentDate"
        `that` (\date -> cfpStatus date { blankConf | cfpEndDate = Just date })
        `is` (\date -> ( Open, Just date ))
        `for` { generator = randomDaTuple
              , shrinker = daTupleShrinker
              }
    , claim
        "cfpStatus is (Closed, Nothing) if currentDate is after cfpEndDate"
        `that` (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate })
        `is` (\_ -> ( Closed, Nothing ))
        `for` { generator =
                  Random.map2 (,) randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( currentDate, endDate ) -> GT == compare' currentDate endDate)
              , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
              }
    , claim
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and cfpStartDate is Nothing"
        `that` (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate })
        `is` (\( _, date ) -> ( Open, Just date ))
        `for` { generator =
                  Random.map2 (,) randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( currentDate, endDate ) -> LT == compare' currentDate endDate)
              , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
              }
    , claim
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and after cfpStartDate"
        `that` (\( currentDate, startDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just currentDate })
        `is` (\( _, _, date ) -> ( Open, Just date ))
        `for` { generator =
                  Random.map3 (,,) randomDaTuple randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( currentDate, _, endDate ) -> LT == compare' currentDate endDate)
                    |> Random.Extra.keepIf (\( currentDate, startDate, _ ) -> GT == compare' currentDate startDate)
              , shrinker = Shrink.tuple3 ( daTupleShrinker, daTupleShrinker, daTupleShrinker )
              }
    , claim
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and equal to cfpStartDate"
        `that` (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just currentDate })
        `is` (\( _, date ) -> ( Open, Just date ))
        `for` { generator =
                  Random.map2 (,) randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( currentDate, endDate ) -> LT == compare' currentDate endDate)
              , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
              }
    , claim
        "cfpStatus is (NotYetOpen, Just cfpStartDate) if currentDate is before cfpEndDate and cfpStartDate"
        `that` (\( currentDate, startDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just startDate })
        `is` (\( _, date, _ ) -> ( NotYetOpen, Just date ))
        `for` { generator =
                  Random.map3 (,,) randomDaTuple randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( _, startDate, endDate ) -> LT == compare' startDate endDate)
                    |> Random.Extra.keepIf (\( currentDate, startDate, _ ) -> LT == compare' currentDate startDate)
              , shrinker = Shrink.tuple3 ( daTupleShrinker, daTupleShrinker, daTupleShrinker )
              }
    ]
