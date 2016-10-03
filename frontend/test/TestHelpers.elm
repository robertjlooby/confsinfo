module TestHelpers exposing (..)

import Date exposing (Month(..))
import DaTuple exposing (DaTuple)
import Lazy.List exposing ((:::), empty)
import Random
import Random.Char
import Random.Date
import Random.Extra
import Random.String
import Shrink exposing (Shrinker)
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


randomTag : Random.Generator Tag
randomTag =
    Random.Extra.sample allTags
        |> Random.map (Maybe.withDefault FunctionalProgramming)


randomListOfTags : Random.Generator (List Tag)
randomListOfTags =
    Random.Extra.rangeLengthList 0 10 randomTag


randomListOfTagsStrings : Random.Generator (List String)
randomListOfTagsStrings =
    Random.map (List.map toString) randomListOfTags


randomWord : Random.Generator String
randomWord =
    Random.String.rangeLengthString 1 100 Random.Char.english


minYear : Int
minYear =
    2000


randomYear : Random.Generator Int
randomYear =
    Random.int minYear 2100


randomDay : Random.Generator Int
randomDay =
    Random.int 1 31


randomDaTuple : Random.Generator DaTuple
randomDaTuple =
    Random.map3 (,,) randomYear Random.Date.month randomDay


yearShrinker : Shrinker Int
yearShrinker =
    Shrink.atLeastInt minYear


monthShrinker : Shrinker Month
monthShrinker month =
    case month of
        Jan ->
            empty

        Feb ->
            Jan ::: monthShrinker Jan

        Mar ->
            Feb ::: monthShrinker Feb

        Apr ->
            Mar ::: monthShrinker Mar

        May ->
            Apr ::: monthShrinker Apr

        Jun ->
            May ::: monthShrinker May

        Jul ->
            Jun ::: monthShrinker Jun

        Aug ->
            Jul ::: monthShrinker Jul

        Sep ->
            Aug ::: monthShrinker Aug

        Oct ->
            Sep ::: monthShrinker Sep

        Nov ->
            Oct ::: monthShrinker Oct

        Dec ->
            Nov ::: monthShrinker Nov


dayShrinker : Shrinker Int
dayShrinker =
    Shrink.atLeastInt 1


daTupleShrinker : Shrink.Shrinker DaTuple
daTupleShrinker =
    Shrink.tuple3 ( yearShrinker, monthShrinker, dayShrinker )
