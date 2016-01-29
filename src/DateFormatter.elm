module DateFormatter (..) where

import Date exposing (Month)
import Date.Core exposing (monthToInt)


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


compare' : DaTuple -> DaTuple -> Order
compare' date date' =
    let
        ( y, m, d ) = date

        ( y', m', d' ) = date'
    in
        if y > y' then
            GT
        else if y < y' then
            LT
        else if (monthToInt m) > (monthToInt m') then
            GT
        else if (monthToInt m) < (monthToInt m') then
            LT
        else
            compare d d'
