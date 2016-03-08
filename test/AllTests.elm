module Main (..) where

import ConferenceTest
import Console
import DaTupleTest
import ElmTest exposing (consoleRunner, Test, suite)
import FilteredTagTest
import FilteredTagSectionTest
import ModelTest
import Task


tests : Test
tests =
  suite
    "All tests"
    [ ConferenceTest.tests
    , DaTupleTest.tests
    , FilteredTagTest.tests
    , FilteredTagSectionTest.tests
    , ModelTest.tests
    ]


port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner tests)
