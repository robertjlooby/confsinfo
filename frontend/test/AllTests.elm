module Main exposing (..)

import ConferenceTest
import DaTupleTest
import ElmTest exposing (runSuite, Test, suite)
import FilteredTagTest
import FilteredTagSectionTest
import ModelTest


tests : Test
tests =
    suite "All tests"
        [ ConferenceTest.tests
        , DaTupleTest.tests
        , FilteredTagTest.tests
        , FilteredTagSectionTest.tests
        , ModelTest.tests
        ]


main : Program Never
main =
    runSuite tests
