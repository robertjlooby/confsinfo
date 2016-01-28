module DateFormatterTest (..) where

import Array
import Check.Investigator as Investigator
import Check.Test as Check
import Date exposing (..)
import Date.Core exposing (monthList, monthToInt, nextMonth)
import DateFormatter exposing (..)
import ElmTest exposing (assertEqual, suite, test)
import Lazy.List exposing ((:::), empty)
import Random
import Random.Extra
import Random.Date
import Shrink exposing (Shrinker)


minYear : Int
minYear =
    2000


maxYear : Int
maxYear =
    2100


intToMonth : Int -> Month
intToMonth int =
    let
        months = Array.fromList monthList

        index = (int - 1) % 12

        maybeMonth = Array.get index months
    in
        case maybeMonth of
            Just m ->
                m

            Nothing ->
                Debug.crash "Month not found!! Should not happen."


randomYearGenerator : Random.Generator Int
randomYearGenerator =
    Random.int minYear maxYear


randomDayGenerator : Random.Generator Int
randomDayGenerator =
    Random.int 1 31


randomMonthGenerator : Random.Generator Month
randomMonthGenerator =
    let
        randomMonthIndex = Random.int 0 11
    in
        Random.map intToMonth randomMonthIndex


randomDaTupleGenerator : Random.Generator DaTuple
randomDaTupleGenerator =
    Random.map3 (,,) randomYearGenerator randomMonthGenerator randomDayGenerator


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
                    Random.map3 (,,) randomDaTupleGenerator randomMonthGenerator randomDayGenerator
                        |> Random.Extra.dropIf unorderedMonths
                , shrinker =
                    Shrink.tuple3 ( daTupleShrinker, monthShrinker, dayShrinker )
                        |> Shrink.dropIf unorderedMonths
                }
                100
                (Random.initialSeed 1)
        , let
            formatDate = (\( y, m, d ) -> toString m ++ " " ++ toString d ++ ", " ++ toString y)

            sameYear = (\( ( y, _, _ ), ( y', _, _ ) ) -> y >= y')
          in
            Check.test
                "formatRange formats days between years"
                (\( date, date' ) -> formatRange date date')
                (\( date, date' ) -> formatDate date ++ "-" ++ formatDate date')
                { generator =
                    Random.map2 (,) randomDaTupleGenerator randomDaTupleGenerator
                        |> Random.Extra.dropIf sameYear
                , shrinker =
                    Shrink.tuple ( daTupleShrinker, daTupleShrinker )
                        |> Shrink.dropIf sameYear
                }
                100
                (Random.initialSeed 1)
        ]
