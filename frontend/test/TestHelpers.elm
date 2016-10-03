module TestHelpers exposing (..)

import Date exposing (Month)
import DaTuple exposing (DaTuple)
import Date.Extra.Core exposing (intToMonth)
import Fuzz exposing (Fuzzer)
import Tag exposing (..)


allTags =
    [ AWS
    , Agile
    , Android
    , AngularJS
    , Arduino
    , Argentina
    , Australia
    , Belarus
    , Belgium
    , BigData
    , Brazil
    , Bulgaria
    , CPlusPlus
    , CSS
    , Canada
    , Chef
    , Chile
    , China
    , Clojure
    , Cloud
    , Cocoa
    , Colombia
    , Communications
    , Croatia
    , CycleJS
    , CzechRepublic
    , DataVisualization
    , Denmark
    , Designers
    , DevOps
    , Developers
    , Diversity
    , Docker
    , DotNet
    , Drupal
    , Elasticsearch
    , Elixir
    , Ember
    , England
    , English
    , Erlang
    , FSharp
    , Finland
    , France
    , French
    , FunctionalProgramming
    , General
    , German
    , Germany
    , Git
    , Go
    , Gradle
    , Grails
    , Groovy
    , Hadoop
    , Haskell
    , Hungary
    , IOS
    , Iceland
    , India
    , InternetOfThings
    , Ireland
    , Israel
    , Italian
    , Italy
    , Japan
    , Japanese
    , Java
    , JavaScript
    , Latvia
    , Lebanon
    , Lisp
    , Lithuania
    , Logstash
    , Malaysia
    , Mexico
    , Microservices
    , Mobile
    , MongoDB
    , Netherlands
    , NewZealand
    , NoSQL
    , NodeJS
    , Norway
    , Norwegian
    , OCaml
    , OpenSource
    , PHP
    , Peru
    , Philippines
    , PhoneGap
    , Poland
    , Polish
    , Portugal
    , PostgreSQL
    , ProgressiveEnhancement
    , PureScript
    , Python
    , Rails
    , RaspberryPi
    , React
    , Remote
    , Robotics
    , Romania
    , Ruby
    , Russia
    , Russian
    , SML
    , SVG
    , Scala
    , Scalability
    , Scotland
    , Security
    , Singapore
    , SoftSkills
    , SoftwareCraftsmanship
    , SouthAfrica
    , SouthKorea
    , Spain
    , Spanish
    , Sweden
    , Swift
    , Switzerland
    , Taiwan
    , Testing
    , Tunisia
    , Turkey
    , Turkish
    , UAE
    , USA
    , UX
    , Ukraine
    , Uruguay
    , Wales
    , Web
    , WordPress
    ]


tagFuzzer : Fuzzer Tag
tagFuzzer =
    List.map Fuzz.constant allTags
        |> List.map ((,) 1)
        |> Fuzz.frequencyOrCrash


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
