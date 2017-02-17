module DaTupleTest exposing (..)

--import Date exposing (Month(..))
--import Date.Extra.Core exposing (monthToInt)

import DaTuple exposing (..)
import Expect
import Test exposing (describe, fuzz)
import Fuzz
import TestHelpers exposing (..)


tests =
    describe "DateFormatter"
        [ fuzz daTupleFuzzer "formatRange formats a single day" <|
            \(( y, m, d ) as date) ->
                (formatRange date date)
                    |> Expect.equal (toString m ++ " " ++ toString d ++ ", " ++ toString y)
        , let
            daysWithinMonthFuzzer =
                Fuzz.map2 (\( y, m, d ) d2 -> ( y, m, d, d + d2 ))
                    daTupleFuzzer
                    dayFuzzer
          in
            fuzz daysWithinMonthFuzzer "formatRange formats days within a month" <|
                \( y, m, d, d2 ) ->
                    (formatRange ( y, m, d ) ( y, m, d2 ))
                        |> Expect.equal (toString m ++ " " ++ toString d ++ "-" ++ toString d2 ++ ", " ++ toString y)
          --        , let
          --            unorderedMonths =
          --                (\( ( _, m, _ ), m', _ ) -> monthToInt m >= monthToInt m')
          --          in
          --            claim "formatRange formats days between months"
          --                `that` (\( ( y, m, d ), m', d' ) -> formatRange ( y, m, d ) ( y, m', d' ))
          --                `is` (\( ( y, m, d ), m', d' ) -> toString m ++ " " ++ toString d ++ "-" ++ toString m' ++ " " ++ toString d' ++ ", " ++ toString y)
          --                `for`
          --                    { generator =
          --                        Random.map3 (,,) randomDaTuple Random.Date.month randomDay
          --                            |> Random.Extra.filter (not << unorderedMonths)
          --                    , shrinker =
          --                        Shrink.tuple3 ( daTupleShrinker, monthShrinker, dayShrinker )
          --                            |> Shrink.dropIf unorderedMonths
          --                    }
          --        , let
          --            formatDate =
          --                (\( y, m, d ) -> toString m ++ " " ++ toString d ++ ", " ++ toString y)
          --
          --            unorderedYears =
          --                (\( ( y, _, _ ), ( y', _, _ ) ) -> y >= y')
          --          in
          --            claim "formatRange formats days between years"
          --                `that` (\( date, date' ) -> formatRange date date')
          --                `is` (\( date, date' ) -> formatDate date ++ "-" ++ formatDate date')
          --                `for`
          --                    { generator =
          --                        Random.map2 (,) randomDaTuple randomDaTuple
          --                            |> Random.Extra.filter (not << unorderedYears)
          --                    , shrinker =
          --                        Shrink.tuple ( daTupleShrinker, daTupleShrinker )
          --                            |> Shrink.dropIf unorderedYears
          --                    }
        , fuzz daTupleFuzzer "compare returns EQ for the same date" <|
            \date ->
                (compareDaTuples date date)
                    |> Expect.equal EQ
          --        , let
          --            sameYear =
          --                (\( ( y, _, _ ), ( y', _, _ ) ) -> y == y')
          --          in
          --            claim "compare compares year first"
          --                `that` (\( date, date' ) -> compare' date date')
          --                `is` (\( ( y, _, _ ), ( y', _, _ ) ) -> compare y y')
          --                `for`
          --                    { generator =
          --                        Random.map2 (,) randomDaTuple randomDaTuple
          --                            |> Random.Extra.filter (not << sameYear)
          --                    , shrinker =
          --                        Shrink.tuple ( daTupleShrinker, daTupleShrinker )
          --                            |> Shrink.dropIf sameYear
          --                    }
          --        , let
          --            sameMonth =
          --                (\( ( _, m, _ ), m', _ ) -> monthToInt m == monthToInt m')
          --          in
          --            claim "compare compares month if year is the same"
          --                `that` (\( ( y, m, d ), m', d' ) -> compare' ( y, m, d ) ( y, m', d' ))
          --                `is` (\( ( _, m, _ ), m', _ ) -> compare (monthToInt m) (monthToInt m'))
          --                `for`
          --                    { generator =
          --                        Random.map3 (,,) randomDaTuple Random.Date.month randomDay
          --                            |> Random.Extra.filter (not << sameMonth)
          --                    , shrinker =
          --                        Shrink.tuple3 ( daTupleShrinker, monthShrinker, dayShrinker )
          --                            |> Shrink.dropIf sameMonth
          --                    }
          --        , let
          --            sameDay =
          --                (\( ( _, _, d ), d' ) -> d == d')
          --          in
          --            claim "compare compares day if year and month are the same"
          --                `that` (\( ( y, m, d ), d' ) -> compare' ( y, m, d ) ( y, m, d' ))
          --                `is` (\( ( _, _, d ), d' ) -> compare d d')
          --                `for`
          --                    { generator =
          --                        Random.map2 (,) randomDaTuple randomDay
          --                            |> Random.Extra.filter (not << sameDay)
          --                    , shrinker =
          --                        Shrink.tuple ( daTupleShrinker, dayShrinker )
          --                            |> Shrink.dropIf sameDay
          --                    }
        ]
