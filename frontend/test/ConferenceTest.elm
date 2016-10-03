module ConferenceTest exposing (..)

import Conference exposing (..)
import Date exposing (Month(..))
import DaTuple as DT
import Expect
import Fuzz
import Tag exposing (Tag(..))
import TestHelpers exposing (..)
import Test exposing (describe, fuzz, fuzz2, fuzz3)


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
    describe "Conference"
        [ fuzz daTupleFuzzer "cfpStatus is (Closed, Nothing) if start and end are Nothing" <|
            \date ->
                cfpStatus date blankConf
                    |> Expect.equal ( Closed, Nothing )
        , fuzz daTupleFuzzer "cfpStatus is False if cfpEndDate is Nothing" <|
            \date ->
                cfpStatus date { blankConf | cfpStartDate = Just date }
                    |> Expect.equal ( Closed, Nothing )
        , fuzz daTupleFuzzer "cfpStatus is (Open, Just cfpEndDate) if cfpEndDate == currentDate" <|
            \date ->
                cfpStatus date { blankConf | cfpEndDate = Just date }
                    |> Expect.equal ( Open, Just date )
          --        , claim "cfpStatus is (Closed, Nothing) if currentDate is after cfpEndDate"
          --            `that` (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate })
          --            `is` (\_ -> ( Closed, Nothing ))
          --            `for` { generator =
          --                        Random.map2 (,) randomDaTuple randomDaTuple
          --                            |> Random.Extra.filter (\( currentDate, endDate ) -> GT == DT.compare' currentDate endDate)
          --                  , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
          --                  }
          --        , claim "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and cfpStartDate is Nothing"
          --            `that` (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate })
          --            `is` (\( _, date ) -> ( Open, Just date ))
          --            `for` { generator =
          --                        Random.map2 (,) randomDaTuple randomDaTuple
          --                            |> Random.Extra.filter (\( currentDate, endDate ) -> LT == DT.compare' currentDate endDate)
          --                  , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
          --                  }
          --        , claim "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and after cfpStartDate"
          --            `that` (\( currentDate, startDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just currentDate })
          --            `is` (\( _, _, date ) -> ( Open, Just date ))
          --            `for` { generator =
          --                        Random.map3 (,,) randomDaTuple randomDaTuple randomDaTuple
          --                            |> Random.Extra.filter (\( currentDate, _, endDate ) -> LT == DT.compare' currentDate endDate)
          --                            |> Random.Extra.filter (\( currentDate, startDate, _ ) -> GT == DT.compare' currentDate startDate)
          --                  , shrinker = Shrink.tuple3 ( daTupleShrinker, daTupleShrinker, daTupleShrinker )
          --                  }
          --        , claim "cfpStatus is (Open, Just cfpEndDate) if currentDate is before cfpEndDate and equal to cfpStartDate"
          --            `that` (\( currentDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just currentDate })
          --            `is` (\( _, date ) -> ( Open, Just date ))
          --            `for` { generator =
          --                        Random.map2 (,) randomDaTuple randomDaTuple
          --                            |> Random.Extra.filter (\( currentDate, endDate ) -> LT == DT.compare' currentDate endDate)
          --                  , shrinker = Shrink.tuple ( daTupleShrinker, daTupleShrinker )
          --                  }
          --        , claim "cfpStatus is (NotYetOpen, Just cfpStartDate) if currentDate is before cfpEndDate and cfpStartDate"
          --            `that` (\( currentDate, startDate, endDate ) -> cfpStatus currentDate { blankConf | cfpEndDate = Just endDate, cfpStartDate = Just startDate })
          --            `is` (\( _, date, _ ) -> ( NotYetOpen, Just date ))
          --            `for` { generator =
          --                        Random.map3 (,,) randomDaTuple randomDaTuple randomDaTuple
          --                            |> Random.Extra.filter (\( _, startDate, endDate ) -> LT == DT.compare' startDate endDate)
          --                            |> Random.Extra.filter (\( currentDate, startDate, _ ) -> LT == DT.compare' currentDate startDate)
          --                  , shrinker = Shrink.tuple3 ( daTupleShrinker, daTupleShrinker, daTupleShrinker )
          --                  }
        , fuzz2 daTupleFuzzer daTupleFuzzer "compare' sorts first by start date" <|
            \d1 d2 ->
                compare' { blankConf | startDate = d1 } { blankConf | startDate = d2 }
                    |> Expect.equal (DT.compare' d1 d2)
        , fuzz3 daTupleFuzzer Fuzz.string Fuzz.string "compare' sorts by name if the start date is the same" <|
            \d n1 n2 ->
                compare' { blankConf | name = n1, startDate = d } { blankConf | name = n2, startDate = d }
                    |> Expect.equal (compare n1 n2)
        ]
