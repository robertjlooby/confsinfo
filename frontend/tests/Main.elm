port module Main exposing (..)

import ConferenceTest
import DateFormatterTest
import FilteredTagTest
import FilteredTagSectionTest
import Json.Encode exposing (Value)
import ModelTest
import Test exposing (Test, describe)
import Test.Runner.Node exposing (TestProgram, run)


tests : Test
tests =
    describe "All tests"
        [ ConferenceTest.tests
        , DateFormatterTest.tests
        , FilteredTagTest.tests
        , FilteredTagSectionTest.tests
        , ModelTest.tests
        ]


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
