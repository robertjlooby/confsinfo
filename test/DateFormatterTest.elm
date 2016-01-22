module DateFormatterTest (..) where

import Array
import Check.Investigator as Investigator
import Check.Test as Check
import Date exposing (..)
import Date.Core exposing (monthList, monthToInt, nextMonth)
import DateFormatter exposing (..)
import ElmTest exposing (assertEqual, suite, test)
import Random
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
    Random.map3 (\y m d -> ( y, m, d )) randomYearGenerator randomMonthGenerator randomDayGenerator


intRangeShrinker : Int -> Int -> Shrinker Int
intRangeShrinker min max =
    Shrink.keepIf (\n -> n >= min && n <= max) Shrink.int


yearShrinker : Shrinker Int
yearShrinker =
    intRangeShrinker minYear maxYear


monthShrinker : Shrinker Month
monthShrinker =
    Shrink.convert intToMonth monthToInt (intRangeShrinker 1 12)


dayShrinker : Shrinker Int
dayShrinker =
    intRangeShrinker 1 31


daTupleShrinker : Shrink.Shrinker DaTuple
daTupleShrinker =
    Shrink.tuple3 ( yearShrinker, monthShrinker, dayShrinker )


daTupleInvestigator : Investigator.Investigator DaTuple
daTupleInvestigator =
    { generator = randomDaTupleGenerator
    , shrinker = daTupleShrinker
    }


tests =
    suite
        "DateFormatter"
        [ Check.test
            "formats a single day"
            (\date -> formatRange date date)
            (\( y, m, d ) -> toString m ++ " " ++ toString d ++ ", " ++ toString y)
            daTupleInvestigator
            100
            (Random.initialSeed 1)
        , Check.test
            "formats days within a month"
            (\( y, m, d ) -> formatRange ( y, m, d ) ( y, m, d + 2 ))
            (\( y, m, d ) -> toString m ++ " " ++ toString d ++ "-" ++ toString (d + 2) ++ ", " ++ toString y)
            daTupleInvestigator
            100
            (Random.initialSeed 1)
        , Check.test
            "formats days between months"
            (\( y, m, d ) -> formatRange ( y, m, d ) ( y, nextMonth m, d ))
            (\( y, m, d ) -> toString m ++ " " ++ toString d ++ "-" ++ toString (nextMonth m) ++ " " ++ toString d ++ ", " ++ toString y)
            daTupleInvestigator
            100
            (Random.initialSeed 1)
        , Check.test
            "formats days between years"
            (\( y, m, d ) -> formatRange ( y, m, d ) ( y + 1, m, d ))
            (\( y, m, d ) -> toString m ++ " " ++ toString d ++ ", " ++ toString y ++ "-" ++ toString m ++ " " ++ toString d ++ ", " ++ toString (y + 1))
            daTupleInvestigator
            100
            (Random.initialSeed 1)
        ]
