module TestHelpers (..) where

import Date exposing (Month(..))
import DateFormatter exposing (DaTuple)
import Lazy.List exposing ((:::), empty)
import Random
import Random.Date
import Shrink exposing (Shrinker)
import Tag exposing (..)


allTags =
  [ AWS
  , Agile
  , Android
  , AngularJS
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
  , Portugal
  , PostgreSQL
  , ProgressiveEnhancement
  , PureScript
  , Python
  , Rails
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
  , WordPress
  ]


minYear : Int
minYear =
  2000


randomYearGenerator : Random.Generator Int
randomYearGenerator =
  Random.int minYear 2100


randomDayGenerator : Random.Generator Int
randomDayGenerator =
  Random.int 1 31


randomDaTupleGenerator : Random.Generator DaTuple
randomDaTupleGenerator =
  Random.map3 (,,) randomYearGenerator Random.Date.month randomDayGenerator


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
