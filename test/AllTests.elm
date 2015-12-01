module Main where

import Console
import ElmTest exposing (consoleRunner, Test, suite)
import Task

tests : Test
tests =
    suite
        "All tests"
            [  ]

port runner : Signal (Task.Task x ())
port runner =
    Console.run (consoleRunner tests)
