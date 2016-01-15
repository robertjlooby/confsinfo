module DateFormatterTest (..) where

import Date exposing (..)
import ElmTest exposing (assertEqual, suite, test)
import DateFormatter exposing (..)


tests =
    suite
        "DateFormatter"
        [ test "formats a single day"
            <| let
                startDate = ( 2016, Jan, 1 )

                endDate = ( 2016, Jan, 1 )
               in
                assertEqual "Jan 1, 2016" <| formatRange startDate endDate
        , test "formats days within a month"
            <| let
                startDate = ( 2016, Jan, 1 )

                endDate = ( 2016, Jan, 3 )
               in
                assertEqual "Jan 1-3, 2016" <| formatRange startDate endDate
        , test "formats days between months"
            <| let
                startDate = ( 2016, Jan, 1 )

                endDate = ( 2016, Feb, 1 )
               in
                assertEqual "Jan 1-Feb 1, 2016" <| formatRange startDate endDate
        , test "formats days between years"
            <| let
                startDate = ( 2016, Jan, 1 )

                endDate = ( 2017, Jan, 1 )
               in
                assertEqual "Jan 1, 2016-Jan 1, 2017" <| formatRange startDate endDate
        ]
