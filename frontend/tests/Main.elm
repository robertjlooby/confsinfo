port module Main exposing (..)

import ConferenceTest
import DaTupleTest
import FilteredTagTest
import FilteredTagSectionTest
import ModelTest
import Json.Encode exposing (Value)
import Test exposing (Test, describe)
import Test.Runner.Node exposing (TestProgram, run)


tests : Test
tests =
    describe "All tests"
        [ ConferenceTest.tests
        , DaTupleTest.tests
        , FilteredTagTest.tests
        , FilteredTagSectionTest.tests
        , ModelTest.tests
        ]


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
