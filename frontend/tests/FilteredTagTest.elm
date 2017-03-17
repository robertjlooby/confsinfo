module FilteredTagTest exposing (..)

import Expect
import FilteredTag exposing (..)
import Fuzz exposing (Fuzzer)
import Lazy.List exposing ((:::), empty)
import Shrink
import Tag exposing (..)
import Test exposing (describe, fuzz, fuzz2)
import TestHelpers exposing (..)


modelFuzzer : Fuzzer (FilteredTag Topic)
modelFuzzer =
    Fuzz.map2 FilteredTag
        tagFuzzer
        Fuzz.bool


tests =
    describe "FilteredTag"
        [ fuzz tagFuzzer "init starts with the tag excluded" <|
            \tag ->
                (init tag)
                    |> Expect.equal { tag = tag, included = False }
        , fuzz modelFuzzer "update with Include includes a model" <|
            \model ->
                update Include model
                    |> .included
                    |> Expect.equal True
        , fuzz modelFuzzer "update with Exclude excludes a model" <|
            \model ->
                update Exclude model
                    |> .included
                    |> Expect.equal False
        , fuzz2 modelFuzzer (Fuzz.list tagFuzzer) "initializeIncludedTag includes a model if its tag is in the list" <|
            \model tags ->
                initializeIncludedTag (model.tag :: tags) model
                    |> .included
                    |> Expect.equal True
        , fuzz2 modelFuzzer (Fuzz.list tagFuzzer) "initializeIncludedTag returns the model if its tag is not in the list" <|
            \model tags ->
                initializeIncludedTag (List.filter ((/=) model.tag) tags) model
                    |> Expect.equal model
        , fuzz modelFuzzer "exclude excludes a model" <|
            \model ->
                exclude model
                    |> .included
                    |> Expect.equal False
        ]
