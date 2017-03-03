module DaTupleTest exposing (..)

import DaTuple exposing (..)
import Expect
import Test exposing (describe, fuzz)
import Fuzz
import TestHelpers exposing (..)


tests =
    describe "DateFormatter"
        [ fuzz daTupleFuzzer "formatRange formats a single day" <|
            \(( y, m, d ) as date) ->
                (formatRange date date)
                    |> Expect.equal (toString m ++ " " ++ toString d ++ ", " ++ toString y)
        , let
            daysWithinMonthFuzzer =
                Fuzz.map2 (\( y, m, d ) d2 -> ( y, m, d, d + d2 ))
                    daTupleFuzzer
                    dayFuzzer
          in
            fuzz daysWithinMonthFuzzer "formatRange formats days within a month" <|
                \( y, m, d, d2 ) ->
                    (formatRange ( y, m, d ) ( y, m, d2 ))
                        |> Expect.equal (toString m ++ " " ++ toString d ++ "-" ++ toString d2 ++ ", " ++ toString y)
        , fuzz daTupleFuzzer "compare returns EQ for the same date" <|
            \date ->
                (compareDaTuples date date)
                    |> Expect.equal EQ
        ]
