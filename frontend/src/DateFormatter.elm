module DateFormatter exposing (formatDate, formatRange, intToMonth)

import Date exposing (Month(..))
import Time.Date exposing (Date, toTuple)


formatDate : Date -> String
formatDate date =
    formatRange date date


formatRange : Date -> Date -> String
formatRange start end =
    let
        ( startYear, startMonth, startDay ) =
            toTuple start

        ( endYear, endMonth, endDay ) =
            toTuple end

        startYearString =
            toString startYear

        startMonthString =
            toString <| intToMonth startMonth

        startDayString =
            toString startDay

        endYearString =
            toString endYear

        endMonthString =
            toString <| intToMonth endMonth

        endDayString =
            toString endDay
    in
        if startYearString /= endYearString then
            startMonthString ++ " " ++ startDayString ++ ", " ++ startYearString ++ "-" ++ endMonthString ++ " " ++ endDayString ++ ", " ++ endYearString
        else if startMonthString == endMonthString && startDayString == endDayString then
            startMonthString ++ " " ++ startDayString ++ ", " ++ startYearString
        else if startMonthString == endMonthString then
            startMonthString ++ " " ++ startDayString ++ "-" ++ endDayString ++ ", " ++ startYearString
        else
            startMonthString ++ " " ++ startDayString ++ "-" ++ endMonthString ++ " " ++ endDayString ++ ", " ++ startYearString


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
