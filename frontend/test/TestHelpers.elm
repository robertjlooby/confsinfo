module TestHelpers exposing (..)

import Date exposing (Month)
import DaTuple exposing (DaTuple)
import Date.Extra.Core exposing (intToMonth)
import Fuzz exposing (Fuzzer)
import Tag exposing (..)


tagFuzzer : Fuzzer Tag
tagFuzzer =
    Fuzz.map2 (\c s -> Tag (String.cons c s)) Fuzz.char Fuzz.string


minYear : Int
minYear =
    2000


yearFuzzer : Fuzzer Int
yearFuzzer =
    Fuzz.intRange minYear 2100


monthFuzzer : Fuzzer Month
monthFuzzer =
    Fuzz.intRange 1 12
        |> Fuzz.map intToMonth


dayFuzzer : Fuzzer Int
dayFuzzer =
    Fuzz.intRange 1 31


daTupleFuzzer : Fuzzer DaTuple
daTupleFuzzer =
    Fuzz.map3 (,,) yearFuzzer monthFuzzer dayFuzzer
