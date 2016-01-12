module Main (..) where

import Console
import ElmTest exposing (consoleRunner, Test, suite)
import Task
import ConferencesTest
import DateFormatterTest


tests : Test
tests =
    suite
        "All tests"
        [ ConferencesTest.tests
        , DateFormatterTest.tests
        ]


port runner : Signal (Task.Task x ())
port runner =
    Console.run (consoleRunner tests)
