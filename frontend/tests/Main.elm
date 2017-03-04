port module Main exposing (..)

import DateFormatterTest
import FilteredTagTest
import FilteredTagSectionTest
import Json.Encode exposing (Value)
import Test exposing (Test, describe)
import Test.Runner.Node exposing (TestProgram, run)


tests : Test
tests =
    describe "All tests"
        [ DateFormatterTest.tests
        , FilteredTagTest.tests
        , FilteredTagSectionTest.tests
        ]


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
