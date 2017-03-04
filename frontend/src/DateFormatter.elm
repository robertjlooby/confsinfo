module DateFormatter exposing (formatRange)

import Date exposing (Month(..))
import Formatting exposing ((<>), any, int, print, s)
import Time.Date exposing (Date, toTuple)


formatRange : Date -> Date -> String
formatRange start end =
    let
        (( startYear, startMonth, startDay ) as startTuple) =
            toTuple start

        (( endYear, endMonth, endDay ) as endTuple) =
            toTuple end
    in
        if start == end then
            formatDay startTuple
        else if startYear /= endYear then
            formatDaysAcrossYears startTuple endTuple
        else if startMonth /= endMonth then
            formatDaysAcrossMonths startTuple endMonth endDay
        else
            formatDaysInMonth startTuple endDay


formatDay : ( Int, Int, Int ) -> String
formatDay ( year, month, day ) =
    let
        format =
            any <> s " " <> int <> s ", " <> int
    in
        print format (intToMonth month) day year


formatDaysInMonth : ( Int, Int, Int ) -> Int -> String
formatDaysInMonth ( year, month, startDay ) endDay =
    let
        format =
            any <> s " " <> int <> s "-" <> int <> s ", " <> int
    in
        print format (intToMonth month) startDay endDay year


formatDaysAcrossMonths : ( Int, Int, Int ) -> Int -> Int -> String
formatDaysAcrossMonths ( year, startMonth, startDay ) endMonth endDay =
    let
        format =
            any <> s " " <> int <> s " - " <> any <> s " " <> int <> s ", " <> int
    in
        print format (intToMonth startMonth) startDay (intToMonth endMonth) endDay year


formatDaysAcrossYears : ( Int, Int, Int ) -> ( Int, Int, Int ) -> String
formatDaysAcrossYears ( startYear, startMonth, startDay ) ( endYear, endMonth, endDay ) =
    let
        format =
            any <> s " " <> int <> s ", " <> int <> s " - " <> any <> s " " <> int <> s ", " <> int
    in
        print format (intToMonth startMonth) startDay startYear (intToMonth endMonth) endDay endYear


{-| Return integer as month. Jan <= 1 Feb == 2 up to Dec > 11.
-}
intToMonth : Int -> Month
intToMonth month =
    if (month <= 1) then
        Jan
    else if (month == 2) then
        Feb
    else if (month == 3) then
        Mar
    else if (month == 4) then
        Apr
    else if (month == 5) then
        May
    else if (month == 6) then
        Jun
    else if (month == 7) then
        Jul
    else if (month == 8) then
        Aug
    else if (month == 9) then
        Sep
    else if (month == 10) then
        Oct
    else if (month == 11) then
        Nov
    else
        Dec
