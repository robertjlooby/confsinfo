module DaTuple exposing (DaTuple, formatDate, formatRange, compareDaTuples)

import Date exposing (Month)
import Date.Extra.Core exposing (monthToInt)


type alias DaTuple =
    ( Int, Month, Int )


formatDate : DaTuple -> String
formatDate date =
    formatRange date date


formatRange : DaTuple -> DaTuple -> String
formatRange ( startYear, startMonth, startDay ) ( endYear, endMonth, endDay ) =
    let
        startYearString =
            toString startYear

        startMonthString =
            toString startMonth

        startDayString =
            toString startDay

        endYearString =
            toString endYear

        endMonthString =
            toString endMonth

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


compareDaTuples : DaTuple -> DaTuple -> Order
compareDaTuples date date2 =
    let
        ( y, m, d ) =
            date

        ( y2, m2, d2 ) =
            date2
    in
        if y > y2 then
            GT
        else if y < y2 then
            LT
        else if (monthToInt m) > (monthToInt m2) then
            GT
        else if (monthToInt m) < (monthToInt m2) then
            LT
        else
            compare d d2
