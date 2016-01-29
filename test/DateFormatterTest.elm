module DateFormatterTest (..) where

import Check.Investigator as Investigator
import Check.Test as Check
import Date exposing (Month(..))
import Date.Core exposing (monthToInt)
import DateFormatter exposing (..)
import ElmTest exposing (suite)
import Lazy.List exposing ((:::), empty)
import Random
import Random.Extra
import Random.Date
import Shrink exposing (Shrinker)


minYear : Int
minYear =
    2000


randomYearGenerator : Random.Generator Int
randomYearGenerator =
    Random.int minYear 2100


randomDayGenerator : Random.Generator Int
randomDayGenerator =
    Random.int 1 31


randomDaTupleGenerator : Random.Generator DaTuple
randomDaTupleGenerator =
    Random.map3 (,,) randomYearGenerator Random.Date.month randomDayGenerator


yearShrinker : Shrinker Int
yearShrinker =
    Shrink.atLeastInt minYear


monthShrinker : Shrinker Month
monthShrinker month =
    case month of
        Jan ->
            empty

        Feb ->
            Jan ::: monthShrinker Jan

        Mar ->
            Feb ::: monthShrinker Feb

        Apr ->
            Mar ::: monthShrinker Mar

        May ->
            Apr ::: monthShrinker Apr

        Jun ->
            May ::: monthShrinker May

        Jul ->
            Jun ::: monthShrinker Jun

        Aug ->
            Jul ::: monthShrinker Jul

        Sep ->
            Aug ::: monthShrinker Aug

        Oct ->
            Sep ::: monthShrinker Sep

        Nov ->
            Oct ::: monthShrinker Oct

        Dec ->
            Nov ::: monthShrinker Nov


dayShrinker : Shrinker Int
dayShrinker =
    Shrink.atLeastInt 1


daTupleShrinker : Shrink.Shrinker DaTuple
daTupleShrinker =
    Shrink.tuple3 ( yearShrinker, monthShrinker, dayShrinker )


tests =
    suite
        "DateFormatter"
        [ Check.test
            "formatRange formats a single day"
            (\date -> formatRange date date)
            (\( y, m, d ) -> toString m ++ " " ++ toString d ++ ", " ++ toString y)
            { generator = randomDaTupleGenerator
            , shrinker = daTupleShrinker
            }
            100
            (Random.initialSeed 1)
        , let
            unorderedDays = (\( ( _, _, d ), d' ) -> d >= d')
          in
            Check.test
                "formatRange formats days within a month"
                (\( ( y, m, d ), d' ) -> formatRange ( y, m, d ) ( y, m, d' ))
                (\( ( y, m, d ), d' ) -> toString m ++ " " ++ toString d ++ "-" ++ toString d' ++ ", " ++ toString y)
                { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDayGenerator
                        |> Random.Extra.dropIf unorderedDays
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, dayShrinker )
                        |> Shrink.dropIf unorderedDays
                }
                100
                (Random.initialSeed 1)
        , let
            unorderedMonths = (\( ( _, m, _ ), m', _ ) -> monthToInt m >= monthToInt m')
          in
            Check.test
                "formatRange formats days between months"
                (\( ( y, m, d ), m', d' ) -> formatRange ( y, m, d ) ( y, m', d' ))
                (\( ( y, m, d ), m', d' ) -> toString m ++ " " ++ toString d ++ "-" ++ toString m' ++ " " ++ toString d' ++ ", " ++ toString y)
                { generator =
                    Random.map3 (,,) randomDaTupleGenerator Random.Date.month randomDayGenerator
                        |> Random.Extra.dropIf unorderedMonths
                , shrinker =
                    Shrink.tuple3 ( daTupleShrinker, monthShrinker, dayShrinker )
                        |> Shrink.dropIf unorderedMonths
                }
                100
                (Random.initialSeed 1)
        , let
            formatDate = (\( y, m, d ) -> toString m ++ " " ++ toString d ++ ", " ++ toString y)

            unorderedYears = (\( ( y, _, _ ), ( y', _, _ ) ) -> y >= y')
          in
            Check.test
                "formatRange formats days between years"
                (\( date, date' ) -> formatRange date date')
                (\( date, date' ) -> formatDate date ++ "-" ++ formatDate date')
                { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDaTupleGenerator
                        |> Random.Extra.dropIf unorderedYears
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, daTupleShrinker )
                        |> Shrink.dropIf unorderedYears
                }
                100
                (Random.initialSeed 1)
        , Check.test
            "compare returns EQ for the same date"
            (\date -> compare' date date)
            (\_ -> EQ)
            { generator = randomDaTupleGenerator
            , shrinker = daTupleShrinker
            }
            100
            (Random.initialSeed 1)
        , let
            sameYear = (\( ( y, _, _ ), ( y', _, _ ) ) -> y == y')
          in
            Check.test
                "compare compares year first"
                (\( date, date' ) -> compare' date date')
                (\( ( y, _, _ ), ( y', _, _ ) ) -> compare y y')
                { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDaTupleGenerator
                        |> Random.Extra.dropIf sameYear
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, daTupleShrinker )
                        |> Shrink.dropIf sameYear
                }
                100
                (Random.initialSeed 1)
        , let
            sameMonth = (\( ( _, m, _ ), m', _ ) -> monthToInt m == monthToInt m')
          in
            Check.test
                "compare compares month if year is the same"
                (\( ( y, m, d ), m', d' ) -> compare' ( y, m, d ) ( y, m', d' ))
                (\( ( _, m, _ ), m', _ ) -> compare (monthToInt m) (monthToInt m'))
                { generator =
                    Random.map3 (,,) randomDaTupleGenerator Random.Date.month randomDayGenerator
                        |> Random.Extra.dropIf sameMonth
                , shrinker =
                    Shrink.tuple3 ( daTupleShrinker, monthShrinker, dayShrinker )
                        |> Shrink.dropIf sameMonth
                }
                100
                (Random.initialSeed 1)
        , let
            sameDay = (\( ( _, _, d ), d' ) -> d == d')
          in
            Check.test
                "compare compares day if year and month are the same"
                (\( ( y, m, d ), d' ) -> compare' ( y, m, d ) ( y, m, d' ))
                (\( ( _, _, d ), d' ) -> compare d d')
                { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDayGenerator
                        |> Random.Extra.dropIf sameDay
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, dayShrinker )
                        |> Shrink.dropIf sameDay
                }
                100
                (Random.initialSeed 1)
        ]
