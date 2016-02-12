module TestHelpers (..) where

import Check.Investigator exposing (Investigator)
import Check.Test
import ElmTest exposing (Test)
import Date exposing (Month(..))
import DateFormatter exposing (DaTuple)
import Lazy.List exposing ((:::), empty)
import Random
import Random.Date
import Shrink exposing (Shrinker)
import Tag exposing (..)


numberOfTests : Int
numberOfTests =
  100


seed : Random.Seed
seed =
  Random.initialSeed 1


checkTest : String -> (a -> b) -> (a -> b) -> Investigator a -> Test
checkTest name actualStatement expectedStatement investigator =
  Check.Test.test name actualStatement expectedStatement investigator numberOfTests seed


allTags =
  [ AWS
  , Agile
  , Android
  , AngularJS
  , Australia
  , Belarus
  , Belgium
  , BigData
  , Bulgaria
  , CPlusPlus
  , CSS
  , Canada
  , Chef
  , China
  , Clojure
  , Cloud
  , Cocoa
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
  , Elasticserch
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
  , Go
  , Gradle
  , Grails
  , Groovy
  , Hadoop
  , Haskell
  , Hungary
  , IOS
  , India
  , InternetOfThings
  , Ireland
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
  , Spain
  , Sweden
  , Swift
  , Switzerland
  , Testing
  , Tunisia
  , Turkey
  , Turkish
  , UAE
  , USA
  , UX
  , Uruguay
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
