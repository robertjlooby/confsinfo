module TestHelpers exposing (..)

import Fuzz exposing (Fuzzer)
import Tag exposing (..)
import Time.Date exposing (Date, date)


tagFuzzer : Fuzzer Topic
tagFuzzer =
    Fuzz.map Topic Fuzz.string


minYear : Int
minYear =
    2000


yearFuzzer : Fuzzer Int
yearFuzzer =
    Fuzz.intRange minYear 2100


monthFuzzer : Fuzzer Int
monthFuzzer =
    Fuzz.intRange 1 12


dayFuzzer : Fuzzer Int
dayFuzzer =
    Fuzz.intRange 1 31


dateFuzzer : Fuzzer Date
dateFuzzer =
    Fuzz.map3 date yearFuzzer monthFuzzer dayFuzzer
