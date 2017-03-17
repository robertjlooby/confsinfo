module ConferenceTest exposing (tests)

import Conference exposing (..)
import Expect
import Json.Decode exposing (decodeString)
import Test exposing (describe, test)
import Time.Date as Date


tests =
    describe "Conference"
        [ test "decodes from JSON" <|
            \() ->
                decodeString decoder "{\"name\": \"name\", \"link\": \"the link\", \"startDate\": \"2016-01-03\", \"endDate\": \"2016-01-05\", \"location\": \"USA\", \"cfpStatus\": {\"status\": \"Open\", \"date\": \"2016-02-08\"}}"
                    |> Expect.equal
                        (Ok
                            { name = "name"
                            , link = "the link"
                            , startDate = Date.date 2016 1 3
                            , endDate = Date.date 2016 1 5
                            , location = "USA"
                            , cfpStatus = Open (Date.date 2016 2 8)
                            }
                        )
        ]
