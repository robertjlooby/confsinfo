module DateFormatterTest (..) where

import Check exposing (claim, for, is, suite, that)
import Check.Test
import Date exposing (Month(..))
import Date.Core exposing (monthToInt)
import DateFormatter exposing (..)
import Lazy.List exposing ((:::), empty)
import Random
import Random.Extra
import Random.Date
import Shrink exposing (Shrinker)
import TestHelpers exposing (..)


tests =
  Check.Test.evidenceToTest <| Check.quickCheck claims


claims : Check.Claim
claims =
  suite
    "DateFormatter"
    [ claim
        "formatRange formats a single day"
        `that` (\date -> formatRange date date)
        `is` (\( y, m, d ) -> toString m ++ " " ++ toString d ++ ", " ++ toString y)
        `for` { generator = randomDaTupleGenerator
              , shrinker = daTupleShrinker
              }
    , let
        unorderedDays =
          (\( ( _, _, d ), d' ) -> d >= d')
      in
        claim
          "formatRange formats days within a month"
          `that` (\( ( y, m, d ), d' ) -> formatRange ( y, m, d ) ( y, m, d' ))
          `is` (\( ( y, m, d ), d' ) -> toString m ++ " " ++ toString d ++ "-" ++ toString d' ++ ", " ++ toString y)
          `for` { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDayGenerator
                      |> Random.Extra.dropIf unorderedDays
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, dayShrinker )
                      |> Shrink.dropIf unorderedDays
                }
    , let
        unorderedMonths =
          (\( ( _, m, _ ), m', _ ) -> monthToInt m >= monthToInt m')
      in
        claim
          "formatRange formats days between months"
          `that` (\( ( y, m, d ), m', d' ) -> formatRange ( y, m, d ) ( y, m', d' ))
          `is` (\( ( y, m, d ), m', d' ) -> toString m ++ " " ++ toString d ++ "-" ++ toString m' ++ " " ++ toString d' ++ ", " ++ toString y)
          `for` { generator =
                    Random.map3 (,,) randomDaTupleGenerator Random.Date.month randomDayGenerator
                      |> Random.Extra.dropIf unorderedMonths
                , shrinker =
                    Shrink.tuple3 ( daTupleShrinker, monthShrinker, dayShrinker )
                      |> Shrink.dropIf unorderedMonths
                }
    , let
        formatDate =
          (\( y, m, d ) -> toString m ++ " " ++ toString d ++ ", " ++ toString y)

        unorderedYears =
          (\( ( y, _, _ ), ( y', _, _ ) ) -> y >= y')
      in
        claim
          "formatRange formats days between years"
          `that` (\( date, date' ) -> formatRange date date')
          `is` (\( date, date' ) -> formatDate date ++ "-" ++ formatDate date')
          `for` { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDaTupleGenerator
                      |> Random.Extra.dropIf unorderedYears
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, daTupleShrinker )
                      |> Shrink.dropIf unorderedYears
                }
    , claim
        "compare returns EQ for the same date"
        `that` (\date -> compare' date date)
        `is` (\_ -> EQ)
        `for` { generator = randomDaTupleGenerator
              , shrinker = daTupleShrinker
              }
    , let
        sameYear =
          (\( ( y, _, _ ), ( y', _, _ ) ) -> y == y')
      in
        claim
          "compare compares year first"
          `that` (\( date, date' ) -> compare' date date')
          `is` (\( ( y, _, _ ), ( y', _, _ ) ) -> compare y y')
          `for` { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDaTupleGenerator
                      |> Random.Extra.dropIf sameYear
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, daTupleShrinker )
                      |> Shrink.dropIf sameYear
                }
    , let
        sameMonth =
          (\( ( _, m, _ ), m', _ ) -> monthToInt m == monthToInt m')
      in
        claim
          "compare compares month if year is the same"
          `that` (\( ( y, m, d ), m', d' ) -> compare' ( y, m, d ) ( y, m', d' ))
          `is` (\( ( _, m, _ ), m', _ ) -> compare (monthToInt m) (monthToInt m'))
          `for` { generator =
                    Random.map3 (,,) randomDaTupleGenerator Random.Date.month randomDayGenerator
                      |> Random.Extra.dropIf sameMonth
                , shrinker =
                    Shrink.tuple3 ( daTupleShrinker, monthShrinker, dayShrinker )
                      |> Shrink.dropIf sameMonth
                }
    , let
        sameDay =
          (\( ( _, _, d ), d' ) -> d == d')
      in
        claim
          "compare compares day if year and month are the same"
          `that` (\( ( y, m, d ), d' ) -> compare' ( y, m, d ) ( y, m, d' ))
          `is` (\( ( _, _, d ), d' ) -> compare d d')
          `for` { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDayGenerator
                      |> Random.Extra.dropIf sameDay
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, dayShrinker )
                      |> Shrink.dropIf sameDay
                }
    ]
