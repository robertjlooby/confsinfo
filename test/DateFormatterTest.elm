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
