module DateFormatterTest exposing (..)

import DateFormatter exposing (..)
import Expect
import Test exposing (describe, test)
import Time.Date as Date


tests =
    describe "DateFormatter"
        [ test "formatRange formats a single day" <|
            \() ->
                let
                    date =
                        Date.date 2016 1 3
                in
                    (formatRange date date)
                        |> Expect.equal "Jan 3, 2016"
        , test "formatRange formats days within a month" <|
            \() ->
                (formatRange (Date.date 2016 1 3) (Date.date 2016 1 5))
                    |> Expect.equal "Jan 3-5, 2016"
        ]