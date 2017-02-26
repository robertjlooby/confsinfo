module FilteredTagTest exposing (..)

import Expect
import FilteredTag exposing (..)
import Fuzz exposing (Fuzzer)
import Lazy.List exposing ((:::), empty)
import Shrink
import Tag exposing (..)
import Test exposing (describe, fuzz, fuzz2)
import TestHelpers exposing (..)


stateFuzzer : Fuzzer State
stateFuzzer =
    Fuzz.frequencyOrCrash
        [ ( 1, Fuzz.constant Included )
        , ( 1, Fuzz.constant Excluded )
        ]


modelFuzzer : Fuzzer Model
modelFuzzer =
    Fuzz.map2 (\tag state -> { tag = tag, state = state })
        tagFuzzer
        stateFuzzer


tests =
    describe "FilteredTag"
        [ fuzz tagFuzzer "init starts with the tag Excluded" <|
            \tag ->
                (init tag)
                    |> Expect.equal { tag = tag, state = Excluded }
        , fuzz modelFuzzer "update with Include includes a model" <|
            \model ->
                update Include model
                    |> .state
                    |> Expect.equal Included
        , fuzz modelFuzzer "update with Exclude excludes a model" <|
            \model ->
                update Exclude model
                    |> .state
                    |> Expect.equal Excluded
        , fuzz2 modelFuzzer (Fuzz.list Fuzz.string) "initializeIncludedTag includes a model if its tag is in the list" <|
            \model strings ->
                case model.tag of
                    Tag tag ->
                        initializeIncludedTag (tag :: strings) model
                            |> .state
                            |> Expect.equal Included
        , fuzz2 modelFuzzer (Fuzz.list Fuzz.string) "initializeIncludedTag returns the model if its tag is not in the list" <|
            \model strings ->
                case model.tag of
                    Tag tag ->
                        initializeIncludedTag (List.filter ((/=) tag) strings) model
                            |> Expect.equal model
        , fuzz modelFuzzer "exclude excludes a model" <|
            \model ->
                exclude model
                    |> .state
                    |> Expect.equal Excluded
        , fuzz modelFuzzer "isIncluded returns True for an Included tag" <|
            \model ->
                { model | state = Included }
                    |> isIncluded
                    |> Expect.equal True
        , fuzz modelFuzzer "isIncluded returns False for an Excluded tag" <|
            \model ->
                { model | state = Excluded }
                    |> isIncluded
                    |> Expect.equal False
        ]
