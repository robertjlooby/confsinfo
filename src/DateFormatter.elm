module DateFormatter (..) where

import Date exposing (Month)


type alias DaTuple =
    ( Int, Month, Int )


formatRange : DaTuple -> DaTuple -> String
formatRange ( startYear, startMonth, startDay ) ( endYear, endMonth, endDay ) =
    let
        startYear' = toString startYear

        startMonth' = toString startMonth

        startDay' = toString startDay

        endYear' = toString endYear

        endMonth' = toString endMonth

        endDay' = toString endDay
    in
        if startYear' /= endYear' then
            startMonth' ++ " " ++ startDay' ++ ", " ++ startYear' ++ "-" ++ endMonth' ++ " " ++ endDay' ++ ", " ++ endYear'
        else if startMonth' == endMonth' && startDay' == endDay' then
            startMonth' ++ " " ++ startDay' ++ ", " ++ startYear'
        else if startMonth' == endMonth' then
            startMonth' ++ " " ++ startDay' ++ "-" ++ endDay' ++ ", " ++ startYear'
        else
            startMonth' ++ " " ++ startDay' ++ "-" ++ endMonth' ++ " " ++ endDay' ++ ", " ++ startYear'
