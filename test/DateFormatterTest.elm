module DateFormatterTest where

import Date
import ElmTest exposing (assertEqual, suite, test)

import DateFormatter exposing (..)


tests =
  suite
    "DateFormatter"
    [
      test "formats a single day" <|
        let startDate = parseDate "1/1/2016"
            endDate = parseDate "1/1/2016"
        in
           assertEqual "Jan 1, 2016" <| formatRange startDate endDate
    , test "formats days within a month" <|
        let startDate = parseDate "1/1/2016"
            endDate = parseDate "1/3/2016"
        in
           assertEqual "Jan 1-3, 2016" <| formatRange startDate endDate
    , test "formats days between months" <|
        let startDate = parseDate "1/1/2016"
            endDate = parseDate "2/1/2016"
        in
           assertEqual "Jan 1-Feb 1, 2016" <| formatRange startDate endDate
    , test "formats days between years" <|
        let startDate = parseDate "1/1/2016"
            endDate = parseDate "1/1/2017"
        in
           assertEqual "Jan 1, 2016-Jan 1, 2017" <| formatRange startDate endDate
    ]
