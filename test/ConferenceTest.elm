module ConferenceTest exposing (..)

import Check exposing (claim, for, is, suite, that)
import Check.Test
import Conference exposing (..)
import ConferenceInternal exposing (..)
import Date exposing (Month(..))
import DaTuple as DT
import Random
import Random.Extra
import Random.String exposing (anyEnglishWord)
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
                    |> Random.Extra.keepIf (\( currentDate, endDate ) -> GT == DT.compare' currentDate endDate)
              , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
              }
    , claim
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and cfpStartDate is Nothing"
        `that` (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate })
        `is` (\( _, date ) -> ( Open, Just date ))
        `for` { generator =
                  Random.map2 (,) randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( currentDate, endDate ) -> LT == DT.compare' currentDate endDate)
              , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
              }
    , claim
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and after cfpStartDate"
        `that` (\( currentDate, startDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just currentDate })
        `is` (\( _, _, date ) -> ( Open, Just date ))
        `for` { generator =
                  Random.map3 (,,) randomDaTuple randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( currentDate, _, endDate ) -> LT == DT.compare' currentDate endDate)
                    |> Random.Extra.keepIf (\( currentDate, startDate, _ ) -> GT == DT.compare' currentDate startDate)
              , shrinker = Shrink.tuple3 ( daTupleShrinker, daTupleShrinker, daTupleShrinker )
              }
    , claim
        "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and equal to cfpStartDate"
        `that` (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just currentDate })
        `is` (\( _, date ) -> ( Open, Just date ))
        `for` { generator =
                  Random.map2 (,) randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( currentDate, endDate ) -> LT == DT.compare' currentDate endDate)
              , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
              }
    , claim
        "cfpStatus is (NotYetOpen, Just cfpStartDate) if currentDate is before cfpEndDate and cfpStartDate"
        `that` (\( currentDate, startDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just startDate })
        `is` (\( _, date, _ ) -> ( NotYetOpen, Just date ))
        `for` { generator =
                  Random.map3 (,,) randomDaTuple randomDaTuple randomDaTuple
                    |> Random.Extra.keepIf (\( _, startDate, endDate ) -> LT == DT.compare' startDate endDate)
                    |> Random.Extra.keepIf (\( currentDate, startDate, _ ) -> LT == DT.compare' currentDate startDate)
              , shrinker = Shrink.tuple3 ( daTupleShrinker, daTupleShrinker, daTupleShrinker )
              }
    , claim
        "compare' sorts first by start date"
        `that` (\( d1, d2 ) -> compare' { blankConf | startDate = d1 } { blankConf | startDate = d2 })
        `is` (\( d1, d2 ) -> DT.compare' d1 d2)
        `for` { generator = Random.map2 (,) randomDaTuple randomDaTuple
              , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
              }
    , claim
        "compare' sorts first by start date"
        `that` (\( d1, d2 ) -> compare' { blankConf | startDate = d1 } { blankConf | startDate = d2 })
        `is` (\( d1, d2 ) -> DT.compare' d1 d2)
        `for` { generator = Random.map2 (,) randomDaTuple randomDaTuple
              , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
              }
    , claim
        "compare' sorts by name if the start date is the same"
        `that` (\( d, n1, n2 ) ->
                  compare'
                    { blankConf | name = n1, startDate = d }
                    { blankConf | name = n2, startDate = d }
               )
        `is` (\( _, n1, n2 ) -> compare n1 n2)
        `for` { generator = Random.map3 (,,) randomDaTuple anyEnglishWord anyEnglishWord
              , shrinker = Shrink.tuple3 ( daTupleShrinker, Shrink.string, Shrink.string )
              }
    ]
