module Conferences where

import Date
import DateFormatter exposing (parseDate)

type alias Model =
  { conferences : List Conference
  , tags : List (String, List (FilteredTag, String))
  }

type alias Conference =
  { name : String
  , link : String
  , startDate : Date.Date
  , endDate : Date.Date
  , location : String
  , tags : List Tag
  }

type Tag =
    AWS
  | Agile
  | Android
  | AngularJS
  | Australia
  | Belarus
  | Belgium
  | BigData
  | Bulgaria
  | CPlusPlus
  | CSS
  | Canada
  | Chef
  | China
  | Clojure
  | Cloud
  | Cocoa
  | Communications
  | CycleJS
  | DataVisualization
  | Denmark
  | Designers
  | DevOps
  | Developers
  | Diversity
  | Docker
  | DotNet
  | Drupal
  | Elasticserch
  | Ember
  | English
  | Erlang
  | FSharp
  | France
  | French
  | FunctionalProgramming
  | General
  | German
  | Germany
  | Go
  | Gradle
  | Grails
  | Groovy
  | Hadoop
  | Haskell
  | Hungary
  | IOS
  | India
  | InternetOfThings
  | Ireland
  | Italian
  | Italy
  | Japan
  | Java
  | JavaScript
  | Lebanon
  | Lithuania
  | Logstash
  | Mexico
  | Microservices
  | Mobile
  | MongoDB
  | Netherlands
  | NewZealand
  | NoSQL
  | NodeJS
  | Norway
  | OCaml
  | OpenSource
  | Philippines
  | PhoneGap
  | Poland
  | Portugal
  | ProgressiveEnhancement
  | PureScript
  | Python
  | Rails
  | React
  | Remote
  | Romania
  | Ruby
  | Russia
  | Russian
  | Scotland
  | SML
  | Scala
  | Scalability
  | Security
  | SoftSkills
  | SoftwareCraftsmanship
  | SouthAfrica
  | Spain
  | Sweden
  | Swift
  | Switzerland
  | UAE
  | Uruguay
  | UK
  | USA
  | UX

type FilteredTag =
  Included Tag
  | Excluded Tag

type Action =
  Include Tag
  | Exclude Tag
  | Reset

update : Action -> Model -> Model
update action model =
  case action of
    Exclude tag ->
      let
        newTags = applyToAllTagsInList (excludeTag tag) model.tags
      in
        { model | tags = newTags }
    Include tag ->
      let
        newTags = applyToAllTagsInList (includeTag tag) model.tags
      in
        { model | tags = newTags }
    Reset ->
      let
        newTags = applyToAllTagsInList excludeAllTags model.tags
      in
        { model | tags = newTags }

applyToAllTagsInList : ((FilteredTag, String) -> (FilteredTag, String)) -> List (String, List (FilteredTag, String)) -> List (String, List (FilteredTag, String))
applyToAllTagsInList transform tagsWithDescription =
  List.map (\(description, tags) -> (description, List.map transform tags)) tagsWithDescription

excludeTag : Tag -> (FilteredTag, String) -> (FilteredTag, String)
excludeTag tag filteredTagWithDisplay =
  let (filteredTag, display) = filteredTagWithDisplay
  in
    case filteredTag of
      Included t ->
        if t == tag then
          (Excluded tag, display)
        else
          filteredTagWithDisplay
      _ -> filteredTagWithDisplay

includeTag : Tag -> (FilteredTag, String) -> (FilteredTag, String)
includeTag tag filteredTagWithDisplay =
  let (filteredTag, display) = filteredTagWithDisplay
  in
    case filteredTag of
      Excluded t ->
        if t == tag then
          (Included tag, display)
        else
          filteredTagWithDisplay
      _ -> filteredTagWithDisplay

excludeAllTags : (FilteredTag, String) -> (FilteredTag, String)
excludeAllTags filteredTagWithDisplay =
  let (filteredTag, display) = filteredTagWithDisplay
  in
    case filteredTag of
      Excluded t -> (Excluded t, display)
      Included t -> (Excluded t, display)

initialize : List String -> Model -> Model
initialize includedTags model =
  let
    filteredTagInitializer = initializeFilteredTag includedTags
    sectionInitializer = (\(section, filteredTagsWithDescription) -> (section, List.map filteredTagInitializer filteredTagsWithDescription))
    newTags = List.map sectionInitializer model.tags
  in
    { model | tags = newTags }

initializeFilteredTag : List String -> (FilteredTag, String) -> (FilteredTag, String)
initializeFilteredTag includedTags (filteredTag, description) =
  let
    tag = getTag filteredTag
  in
    if List.member (toString tag) includedTags then
      (Included tag, description)
    else
      (filteredTag, description)

includedTags : Model -> List String
includedTags model =
  List.map (\(_, filteredTagsWithDescriptions) -> filteredTagsWithDescriptions) model.tags
    |> List.concat
    |> List.map (\(filteredTag, _) -> filteredTag)
    |> List.filter tagIsIncluded
    |> List.map getTag
    |> List.map toString

shouldShow : List FilteredTag -> List Conference -> List Conference
shouldShow tags conferences =
  let
    filteredTagsToShow = List.filter tagIsIncluded tags
    tagsToShow = List.map getTag filteredTagsToShow
  in
    List.filter (shouldShowConference tagsToShow) conferences

shouldShowConference : List Tag -> Conference -> Bool
shouldShowConference tags conference =
  List.all (\tag -> List.member tag conference.tags) tags

tagIsIncluded : FilteredTag -> Bool
tagIsIncluded tag =
  case tag of
    Included _ -> True
    Excluded _ -> False

getTag : FilteredTag -> Tag
getTag tag =
  case tag of
    Included t -> t
    Excluded t -> t

list : Model
list =
  { conferences =
      [
        { name = "@Swift"
        , link = "http://atswift.io/index-en.html"
        , startDate = parseDate "2016-1-10"
        , endDate = parseDate "2016-1-10"
        , location = "Beijing, China"
        , tags = [English, Developers, China, Swift, IOS]
        }
      , { name = "NDC London Workshops"
        , link = "http://ndc-london.com/"
        , startDate = parseDate "2016-1-11"
        , endDate = parseDate "2016-1-12"
        , location = "London, UK"
        , tags = [English, Developers, UK, Agile, DotNet, General]
        }
      , { name = "NDC London"
        , link = "http://ndc-london.com/"
        , startDate = parseDate "2016-1-13"
        , endDate = parseDate "2016-1-15"
        , location = "London, UK"
        , tags = [English, Developers, UK, Agile, DotNet, General]
        }
      , { name = "Internet of Things Milan"
        , link = "https://www.mongodb.com/events/internet-of-things-milan"
        , startDate = parseDate "2016-1-14"
        , endDate = parseDate "2016-1-14"
        , location = "Milan, Italy"
        , tags = [English, Developers, Italy, MongoDB, BigData, InternetOfThings]
        }
      , { name = "GR8Conf IN"
        , link = "http://gr8conf.in/"
        , startDate = parseDate "2016-1-16"
        , endDate = parseDate "2016-1-16"
        , location = "New Delhi, India"
        , tags = [English, Developers, India, Java, Groovy, Grails, Gradle]
        }
      , { name = "PhoneGap Day"
        , link = "http://pgday.phonegap.com/us2016/"
        , startDate = parseDate "2016-1-28"
        , endDate = parseDate "2016-1-28"
        , location = "Lehi, UT"
        , tags = [English, Developers, USA, PhoneGap, Mobile]
        }
      , { name = "/dev/winter"
        , link = "http://devcycles.net/2016/winter/"
        , startDate = parseDate "2016-1-23"
        , endDate = parseDate "2016-1-23"
        , location = "Cambridge, UK"
        , tags = [English, Developers, UK, DevOps, NoSQL, FunctionalProgramming]
        }
      , { name = "dotSwift"
        , link = "http://www.dotswift.io/"
        , startDate = parseDate "2016-1-29"
        , endDate = parseDate "2016-1-29"
        , location = "Paris, France"
        , tags = [English, Developers, France, Swift]
        }
      , { name = "AlterConf D.C."
        , link = "http://www.alterconf.com/sessions/washington-dc"
        , startDate = parseDate "2016-1-30"
        , endDate = parseDate "2016-1-30"
        , location = "Washington, DC"
        , tags = [English, Designers, Developers, USA, Diversity, SoftSkills]
        }
      , { name = "FOSDEM"
        , link = "https://fosdem.org/2016/"
        , startDate = parseDate "2016-1-30"
        , endDate = parseDate "2016-1-31"
        , location = "Brussels, Belgium"
        , tags = [English, Developers, Belgium, OpenSource, General]
        }
      , { name = "UX/DEV Summit"
        , link = "http://uxdsummit.com/"
        , startDate = parseDate "2016-2-4"
        , endDate = parseDate "2016-2-6"
        , location = "Fort Lauderdale, FL"
        , tags = [English, Designers, Developers, USA, UX, AngularJS, Ember, React]
        }
      , { name = "Compose 2016"
        , link = "http://www.composeconference.org/"
        , startDate = parseDate "2016-2-4"
        , endDate = parseDate "2016-2-5"
        , location = "Brooklyn, NY"
        , tags = [English, Developers, USA, FunctionalProgramming, Haskell, FSharp, OCaml, SML]
        }
      , { name = "RubyFuza"
        , link = "http://www.rubyfuza.org/"
        , startDate = parseDate "2016-2-4"
        , endDate = parseDate "2016-2-5"
        , location = "Cape Town, South Africa"
        , tags = [English, Developers, SouthAfrica, Ruby]
        }
      , { name = "The Microservices Conference"
        , link = "http://microxchg.io/2016/"
        , startDate = parseDate "2016-2-4"
        , endDate = parseDate "2016-2-5"
        , location = "Berlin, Germany"
        , tags = [English, Developers, Germany, Microservices]
        }
      , { name = "JSConf Beirut"
        , link = "http://www.jsconfbeirut.com/"
        , startDate = parseDate "2016-2-6"
        , endDate = parseDate "2016-2-7"
        , location = "Beirut, Lebanon"
        , tags = [English, Developers, Lebanon, JavaScript]
        }
      , { name = "Forward JS Workshops"
        , link = "http://forwardjs.com/home"
        , startDate = parseDate "2016-2-8"
        , endDate = parseDate "2016-2-9"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, JavaScript, AngularJS, React, NodeJS]
        }
      , { name = "Webstock"
        , link = "http://www.webstock.org.nz/16/"
        , startDate = parseDate "2016-2-9"
        , endDate = parseDate "2016-2-12"
        , location = "Wellington, New Zealand"
        , tags = [English, Designers, Developers, NewZealand, General]
        }
      , { name = "Forward JS"
        , link = "http://forwardjs.com/home"
        , startDate = parseDate "2016-2-10"
        , endDate = parseDate "2016-2-10"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, JavaScript, AngularJS, React, NodeJS]
        }
      , { name = "RubyConf Australia"
        , link = "http://www.rubyconf.org.au/2016"
        , startDate = parseDate "2016-2-10"
        , endDate = parseDate "2016-2-13"
        , location = "Gold Coast, Australia"
        , tags = [English, Developers, Australia, Ruby]
        }
      , { name = "Clojure Remote"
        , link = "http://clojureremote.com/"
        , startDate = parseDate "2016-2-11"
        , endDate = parseDate "2016-2-12"
        , location = "Remote"
        , tags = [English, Developers, Remote, Clojure, FunctionalProgramming]
        }
      , { name = "Forward JS Workshops"
        , link = "http://forwardjs.com/home"
        , startDate = parseDate "2016-2-11"
        , endDate = parseDate "2016-2-13"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, JavaScript, AngularJS, React, NodeJS]
        }
      , { name = "DevNexus"
        , link = "http://devnexus.com/s/index"
        , startDate = parseDate "2016-2-15"
        , endDate = parseDate "2016-2-17"
        , location = "Atlanta, GA"
        , tags = [English, Developers, USA, Agile, BigData, Java, JavaScript, General]
        }
      , { name = "Elastic{ON}"
        , link = "https://www.elastic.co/elasticon"
        , startDate = parseDate "2016-2-17"
        , endDate = parseDate "2016-2-19"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, MongoDB, BigData, Elasticserch, Logstash]
        }
      , { name = "Lambda Days"
        , link = "http://www.lambdadays.org/"
        , startDate = parseDate "2016-2-18"
        , endDate = parseDate "2016-2-19"
        , location = "Krakow, Poland"
        , tags = [English, Developers, Poland, FunctionalProgramming, Erlang]
        }
      , { name = "BOB 2016"
        , link = "http://bobkonf.de/2016/en/"
        , startDate = parseDate "2016-2-19"
        , endDate = parseDate "2016-2-19"
        , location = "Berlin, Germany"
        , tags = [English, German, Developers, Germany, FunctionalProgramming, Clojure, Haskell, Scala, Erlang]
        }
      , { name = "GopherCon India"
        , link = "http://www.gophercon.in/"
        , startDate = parseDate "2016-2-19"
        , endDate = parseDate "2016-2-10"
        , location = "Bengaluru, India"
        , tags = [English, Developers, India, Go]
        }
      , { name = ":clojureD"
        , link = "http://www.clojured.de/"
        , startDate = parseDate "2016-2-20"
        , endDate = parseDate "2016-2-20"
        , location = "Berlin, Germany"
        , tags = [English, Developers, Germany, FunctionalProgramming, Clojure]
        }
      , { name = "The Rolling Scopes Conference"
        , link = "http://2016.conf.rollingscopes.com/"
        , startDate = parseDate "2016-2-20"
        , endDate = parseDate "2016-2-21"
        , location = "Minsk, Belarus"
        , tags = [English, Russian, Developers, Belarus, CSS, NodeJS, JavaScript]
        }
      , { name = "React.js Conf"
        , link = "http://conf.reactjs.com/"
        , startDate = parseDate "2016-2-22"
        , endDate = parseDate "2016-2-23"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, React, JavaScript]
        }
      , { name = "GopherCon Dubai"
        , link = "http://www.gophercon.ae/"
        , startDate = parseDate "2016-2-23"
        , endDate = parseDate "2016-2-23"
        , location = "Dubai, UAE"
        , tags = [English, Developers, UAE, Go]
        }
      , { name = "ConFoo"
        , link = "http://confoo.ca/en"
        , startDate = parseDate "2016-2-24"
        , endDate = parseDate "2016-2-26"
        , location = "Montreal, QC, Canada"
        , tags = [English, French, Developers, Canada, General]
        }
      , { name = "try! Swift"
        , link = "http://www.tryswiftconf.com/en"
        , startDate = parseDate "2016-3-2"
        , endDate = parseDate "2016-3-4"
        , location = "Tokyo, Japan"
        , tags = [English, Developers, Japan, Swift, IOS]
        }
      , { name = "EnhanceConf"
        , link = "http://enhanceconf.com/"
        , startDate = parseDate "2016-3-3"
        , endDate = parseDate "2016-3-4"
        , location = "London, UK"
        , tags = [English, Developers, UK, CSS, JavaScript, ProgressiveEnhancement]
        }
      , { name = "Big Data Paris"
        , link = "http://www.bigdataparis.com/"
        , startDate = parseDate "2016-3-7"
        , endDate = parseDate "2016-3-8"
        , location = "Paris, France"
        , tags = [French, Developers, France, MongoDB, BigData]
        }
      , { name = "Erlang Factory SF Workshops"
        , link = "http://www.erlang-factory.com/sfbay2016/home"
        , startDate = parseDate "2016-3-7"
        , endDate = parseDate "2016-3-9"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, Erlang]
        }
      , { name = "Fluent Conf Trainings"
        , link = "http://conferences.oreilly.com/fluent/javascript-html-us"
        , startDate = parseDate "2016-3-7"
        , endDate = parseDate "2016-3-8"
        , location = "San Francisco, CA"
        , tags = [English, Designers, Developers, USA, CSS, JavaScript, UX, React, AngularJS, Docker]
        }
      , { name = "QCon London"
        , link = "http://qconlondon.com/"
        , startDate = parseDate "2016-3-7"
        , endDate = parseDate "2016-3-9"
        , location = "London, UK"
        , tags = [English, Developers, UK, General]
        }
      , { name = "Fluent Conf"
        , link = "http://conferences.oreilly.com/fluent/javascript-html-us"
        , startDate = parseDate "2016-3-8"
        , endDate = parseDate "2016-3-10"
        , location = "San Francisco, CA"
        , tags = [English, Designers, Developers, USA, CSS, JavaScript, UX, React, AngularJS, Docker]
        }
      , { name = "jDays"
        , link = "http://www.jdays.se/"
        , startDate = parseDate "2016-3-8"
        , endDate = parseDate "2016-3-9"
        , location = "Gothenburg, Sweden"
        , tags = [English, Developers, Sweden, Java]
        }
      , { name = "SoCraTes ES"
        , link = "http://socrates-conference.es/doku.php"
        , startDate = parseDate "2016-3-10"
        , endDate = parseDate "2016-3-13"
        , location = "Gran Canaria, Spain"
        , tags = [English, Developers, Spain, SoftwareCraftsmanship]
        }
      , { name = "Erlang Factory SF"
        , link = "http://www.erlang-factory.com/sfbay2016/home"
        , startDate = parseDate "2016-3-10"
        , endDate = parseDate "2016-3-11"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, Erlang]
        }
      , { name = "QCon London Tutorials"
        , link = "http://qconlondon.com/"
        , startDate = parseDate "2016-3-10"
        , endDate = parseDate "2016-3-11"
        , location = "London, UK"
        , tags = [English, Developers, UK, General]
        }
      , { name = "ScaleConf"
        , link = "http://scaleconf.org/"
        , startDate = parseDate "2016-3-10"
        , endDate = parseDate "2016-3-11"
        , location = "Cape Town, South Africa"
        , tags = [English, Developers, SouthAfrica, Scalability]
        }
      , { name = "Frontier Conf"
        , link = "https://www.frontierconf.com/"
        , startDate = parseDate "2016-3-11"
        , endDate = parseDate "2016-3-11"
        , location = "London, UK"
        , tags = [English, Designers, UK, CSS, UX]
        }
      , { name = "RWDevCon 2016"
        , link = "http://rwdevcon.com/"
        , startDate = parseDate "2016-3-11"
        , endDate = parseDate "2016-3-12"
        , location = "Alexandria, VA"
        , tags = [English, Developers, USA, IOS, Swift]
        }
      , { name = "Bath Ruby"
        , link = "http://bathruby.us1.list-manage1.com/subscribe?u=f18bb7508370614edaffb50dd&id=73743da027"
        , startDate = parseDate "2016-3-11"
        , endDate = parseDate "2016-3-11"
        , location = "Bath, UK"
        , tags = [English, Developers, UK, Ruby]
        }
      , { name = "wroc_love.rb"
        , link = "http://www.wrocloverb.com/"
        , startDate = parseDate "2016-3-11"
        , endDate = parseDate "2016-3-13"
        , location = "Wroclaw, Poland"
        , tags = [English, Developers, Poland, Ruby]
        }
      , { name = "An Event Apart Nashville"
        , link = "http://aneventapart.com/event/nashville-2016"
        , startDate = parseDate "2016-3-14"
        , endDate = parseDate "2016-3-16"
        , location = "Nashville, TN"
        , tags = [English, Designers, Developers, USA, UX]
        }
      , { name = "Erlang Factory SF Workshops"
        , link = "http://www.erlang-factory.com/sfbay2016/home"
        , startDate = parseDate "2016-3-14"
        , endDate = parseDate "2016-3-16"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, Erlang]
        }
      , { name = "Smashing Conf"
        , link = "http://smashingconf.com/"
        , startDate = parseDate "2016-3-15"
        , endDate = parseDate "2016-3-16"
        , location = "Oxford, UK"
        , tags = [English, Designers, UK, UX]
        }
      , { name = "droidcon San Francisco"
        , link = "http://sf.droidcon.com/"
        , startDate = parseDate "2016-3-17"
        , endDate = parseDate "2016-3-18"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, Android, Mobile]
        }
      , { name = "mdevcon"
        , link = "http://mdevcon.com/"
        , startDate = parseDate "2016-3-17"
        , endDate = parseDate "2016-3-18"
        , location = "Amsterdam, Netherlands"
        , tags = [English, Developers, Netherlands, Mobile, IOS, Android]
        }
      , { name = "Scale Summit"
        , link = "http://www.scalesummit.org/"
        , startDate = parseDate "2016-3-18"
        , endDate = parseDate "2016-3-18"
        , location = "London, UK"
        , tags = [English, Developers, UK, Scalability]
        }
      , { name = "RubyConf India"
        , link = "http://rubyconfindia.org/"
        , startDate = parseDate "2016-3-19"
        , endDate = parseDate "2016-3-20"
        , location = "Kochi, India"
        , tags = [English, Developers, India, Ruby]
        }
      , { name = "CSS Day"
        , link = "http://2016.cssday.it/"
        , startDate = parseDate "2016-3-25"
        , endDate = parseDate "2016-3-25"
        , location = "Faenza, Italy"
        , tags = [Italian, Designers, Italy, CSS]
        }
      , { name = "DevExperience"
        , link = "http://devexperience.ro/"
        , startDate = parseDate "2016-3-25"
        , endDate = parseDate "2016-3-25"
        , location = "Lasi, Romania"
        , tags = [English, Developers, Romania, General]
        }
      , { name = "CocoaConf Chicago"
        , link = "http://cocoaconf.com/chicago-2016/home"
        , startDate = parseDate "2016-3-25"
        , endDate = parseDate "2016-3-26"
        , location = "Chicago, IL"
        , tags = [English, Developers, USA, Cocoa, IOS]
        }
      , { name = "Space City JS"
        , link = "http://spacecity.codes/"
        , startDate = parseDate "2016-3-26"
        , endDate = parseDate "2016-3-26"
        , location = "Houston, TX"
        , tags = [English, Developers, USA, JavaScript]
        }
      , { name = "EmberConf"
        , link = "http://emberconf.com/"
        , startDate = parseDate "2016-3-29"
        , endDate = parseDate "2016-3-30"
        , location = "Portland, OR"
        , tags = [English, Developers, USA, Ember]
        }
      , { name = "Clarity Conf"
        , link = "http://clarityconf.com/"
        , startDate = parseDate "2016-3-31"
        , endDate = parseDate "2016-4-1"
        , location = "San Francisco, CA"
        , tags = [English, Designers, USA, UX]
        }
      , { name = "Ruby on Ales"
        , link = "https://ruby.onales.com/"
        , startDate = parseDate "2016-3-31"
        , endDate = parseDate "2016-4-1"
        , location = "Bend, OR"
        , tags = [English, Developers, USA, Ruby]
        }
      , { name = "Fronteers Spring Thing"
        , link = "https://fronteers.nl/spring"
        , startDate = parseDate "2016-4-1"
        , endDate = parseDate "2016-4-1"
        , location = "Amsterdam, Netherlands"
        , tags = [English, Developers, Netherlands, JavaScript]
        }
      , { name = "An Event Apart Seattle"
        , link = "http://aneventapart.com/event/seattle-2016"
        , startDate = parseDate "2016-4-4"
        , endDate = parseDate "2016-4-6"
        , location = "Seattle, WA"
        , tags = [English, Designers, Developers, USA, UX]
        }
      , { name = "Smashing Conf SF"
        , link = "http://smashingconf.com/sf-2016/"
        , startDate = parseDate "2016-4-5"
        , endDate = parseDate "2016-4-6"
        , location = "San Francisco, CA"
        , tags = [English, Designers, USA, UX]
        }
      , { name = "Ancient City Ruby"
        , link = "http://www.ancientcityruby.com/"
        , startDate = parseDate "2016-4-6"
        , endDate = parseDate "2016-4-8"
        , location = "St. Augustine, FL"
        , tags = [English, Developers, USA, Ruby]
        }
      , { name = "AlterConf Minneapolis"
        , link = "http://www.alterconf.com/sessions/minneapolis-mn"
        , startDate = parseDate "2016-4-9"
        , endDate = parseDate "2016-4-9"
        , location = "Minneapolis, MN"
        , tags = [English, Designers, Developers, USA, Diversity, SoftSkills]
        }
      , { name = "RubyConf Philippines"
        , link = "http://rubyconf.ph/"
        , startDate = parseDate "2016-4-7"
        , endDate = parseDate "2016-4-9"
        , location = "Taguig, Philippines"
        , tags = [English, Developers, Philippines, Ruby]
        }
      , { name = "Greach"
        , link = "http://greachconf.com/"
        , startDate = parseDate "2016-4-8"
        , endDate = parseDate "2016-4-9"
        , location = "Madrid, Spain"
        , tags = [English, Developers, Spain, Java, Groovy, Grails, Gradle]
        }
      , { name = "MobCon"
        , link = "http://mobcon.com/mobcon-europe/"
        , startDate = parseDate "2016-4-10"
        , endDate = parseDate "2016-4-10"
        , location = "Sofia, Bulgaria"
        , tags = [English, Developers, Bulgaria, Mobile, IOS, Android]
        }
      , { name = "Converge SE"
        , link = "http://convergese.com/"
        , startDate = parseDate "2016-4-13"
        , endDate = parseDate "2016-4-15"
        , location = "Columbia, SC"
        , tags = [English, Designers, Developers, USA, General]
        }
      , { name = "Hadoop Summit Europe"
        , link = "http://2016.hadoopsummit.org/dublin/"
        , startDate = parseDate "2016-4-13"
        , endDate = parseDate "2016-4-14"
        , location = "Dublin, Ireland"
        , tags = [English, Developers, Ireland, Hadoop, BigData, InternetOfThings]
        }
      , { name = "ACE! Conference"
        , link = "http://aceconf.com/"
        , startDate = parseDate "2016-4-14"
        , endDate = parseDate "2016-4-15"
        , location = "Krakow, Poland"
        , tags = [English, Developers, Poland, Agile, SoftwareCraftsmanship]
        }
      , { name = "Clojure/west"
        , link = "http://clojurewest.org/"
        , startDate = parseDate "2016-4-15"
        , endDate = parseDate "2016-4-16"
        , location = "Seattle, WA"
        , tags = [English, Developers, USA, Clojure, FunctionalProgramming]
        }
      , { name = "CocoaConf Austin"
        , link = "http://cocoaconf.com/austin-2016/home"
        , startDate = parseDate "2016-4-15"
        , endDate = parseDate "2016-4-16"
        , location = "Austin, TX"
        , tags = [English, Developers, USA, Cocoa, IOS]
        }
      , { name = "JSConf Uruguay"
        , link = "https://jsconf.uy/"
        , startDate = parseDate "2016-4-15"
        , endDate = parseDate "2016-4-16"
        , location = "Montevideo, Uruguay"
        , tags = [English, Developers, Uruguay, JavaScript]
        }
      , { name = "Scalar"
        , link = "http://scalar-conf.com/"
        , startDate = parseDate "2016-4-16"
        , endDate = parseDate "2016-4-16"
        , location = "Warsaw, Poland"
        , tags = [English, Developers, Poland, Scala]
        }
      , { name = "ACCU"
        , link = "http://accu.org/index.php/conferences/accu_conference_2016"
        , startDate = parseDate "2016-4-19"
        , endDate = parseDate "2016-4-23"
        , location = "Bristol, UK"
        , tags = [English, Developers, UK, General, CPlusPlus]
        }
      , { name = "Industry Conf"
        , link = "http://2016.industryconf.com/"
        , startDate = parseDate "2016-4-20"
        , endDate = parseDate "2016-4-20"
        , location = "Newcastle, UK"
        , tags = [English, Developers, UK, General]
        }
      , { name = "CycleConf"
        , link = "https://twitter.com/cycleconf"
        , startDate = parseDate "2016-4-21"
        , endDate = parseDate "2016-4-24"
        , location = "Copenhagen, Denmark"
        , tags = [English, Developers, Denmark, CycleJS, JavaScript]
        }
      , { name = "MCE"
        , link = "http://mceconf.com/"
        , startDate = parseDate "2016-4-21"
        , endDate = parseDate "2016-4-22"
        , location = "Warsaw, Poland"
        , tags = [English, Designers, Developers, Poland, Mobile]
        }
      , { name = "Render Conf"
        , link = "http://2016.render-conf.com/"
        , startDate = parseDate "2016-4-21"
        , endDate = parseDate "2016-4-22"
        , location = "Oxford, UK"
        , tags = [English, Designers, Developers, UK, CSS, JavaScript, UX]
        }
      , { name = "dotSecurity"
        , link = "http://www.dotsecurity.io/"
        , startDate = parseDate "2016-4-22"
        , endDate = parseDate "2016-4-22"
        , location = "Paris, France"
        , tags = [English, Developers, France, Security]
        }
      , { name = "Generate NY"
        , link = "http://generateconf.com/new-york-2016"
        , startDate = parseDate "2016-4-22"
        , endDate = parseDate "2016-4-22"
        , location = "New York, NY"
        , tags = [English, Designers, USA, UX]
        }
      , { name = "Future of Web Design"
        , link = "https://futureofwebdesign.com/london-2016/"
        , startDate = parseDate "2016-4-25"
        , endDate = parseDate "2016-4-27"
        , location = "London, UK"
        , tags = [English, Designers, UK, UX]
        }
      , { name = "dotScale"
        , link = "http://www.dotscale.io/"
        , startDate = parseDate "2016-4-25"
        , endDate = parseDate "2016-4-25"
        , location = "Paris, France"
        , tags = [English, Developers, France, Scalability]
        }
      , { name = "OpenVis Conf"
        , link = "https://openvisconf.com/"
        , startDate = parseDate "2016-4-25"
        , endDate = parseDate "2016-4-26"
        , location = "Boston, MA"
        , tags = [English, Developers, USA, DataVisualization]
        }
      , { name = "Craft Conf"
        , link = "http://craft-conf.com/2016"
        , startDate = parseDate "2016-4-26"
        , endDate = parseDate "2016-4-29"
        , location = "Budapest, Hungary"
        , tags = [English, Developers, Hungary, SoftwareCraftsmanship]
        }
      , { name = "University of Illinois WebCon"
        , link = "http://webcon.illinois.edu/"
        , startDate = parseDate "2016-4-27"
        , endDate = parseDate "2016-4-28"
        , location = "Champaign, IL"
        , tags = [English, Developers, USA, General]
        }
      , { name = "YOW! Lambda Jam"
        , link = "http://lambdajam.yowconference.com.au/"
        , startDate = parseDate "2016-4-28"
        , endDate = parseDate "2016-4-29"
        , location = "Brisbane, Australia"
        , tags = [English, Developers, Australia, FunctionalProgramming]
        }
      , { name = "Future Insights Live"
        , link = "https://futureinsightslive.com/chicago-2016/"
        , startDate = parseDate "2016-5-2"
        , endDate = parseDate "2016-5-5"
        , location = "Chicago, IL"
        , tags = [English, Designers, Developers, USA, UX, JavaScript, InternetOfThings]
        }
      , { name = "YOW! West"
        , link = "http://west.yowconference.com.au/"
        , startDate = parseDate "2016-5-3"
        , endDate = parseDate "2016-5-4"
        , location = "Perth, Australia"
        , tags = [English, Developers, Australia, General]
        }
      , { name = "ng-conf"
        , link = "http://www.ng-conf.org/"
        , startDate = parseDate "2016-5-4"
        , endDate = parseDate "2016-5-6"
        , location = "Salt Lake City, UT"
        , tags = [English, Developers, USA, JavaScript, AngularJS]
        }
      , { name = "RailsConf"
        , link = "http://railsconf.com/"
        , startDate = parseDate "2016-5-4"
        , endDate = parseDate "2016-5-6"
        , location = "Kansas City, MO"
        , tags = [English, Developers, USA, Ruby, Rails]
        }
      , { name = "CocoaConf Seattle"
        , link = "http://cocoaconf.com/seattle-2016/home"
        , startDate = parseDate "2016-5-6"
        , endDate = parseDate "2016-5-7"
        , location = "Seattle, WA"
        , tags = [English, Developers, USA, IOS, Cocoa]
        }
      , { name = "SyntaxCon"
        , link = "http://2016.syntaxcon.com/"
        , startDate = parseDate "2016-5-6"
        , endDate = parseDate "2016-5-7"
        , location = "Charleston, SC"
        , tags = [English, Developers, USA, General]
        }
      , { name = "C++Now"
        , link = "http://cppnow.org/"
        , startDate = parseDate "2016-5-9"
        , endDate = parseDate "2016-5-14"
        , location = "Aspen, CO"
        , tags = [English, Developers, USA, CPlusPlus]
        }
      , { name = "Apache: Big Data North America"
        , link = "http://www.apachecon.com/"
        , startDate = parseDate "2016-5-9"
        , endDate = parseDate "2016-5-11"
        , location = "Vancouver, BC, Canada"
        , tags = [English, Developers, Canada, BigData]
        }
      , { name = "Beyond Tellerrand"
        , link = "http://beyondtellerrand.com/events/duesseldorf-2016"
        , startDate = parseDate "2016-5-9"
        , endDate = parseDate "2016-5-11"
        , location = "Düsseldorf, Germany"
        , tags = [English, Designers, Developers, Germany, UX, General]
        }
      , { name = "ChefConf"
        , link = "https://www.chef.io/chefconf/"
        , startDate = parseDate "2016-5-9"
        , endDate = parseDate "2016-5-11"
        , location = "Austin, TX"
        , tags = [English, Developers, USA, Chef, DevOps]
        }
      , { name = "jsDay"
        , link = "http://2016.jsday.it/"
        , startDate = parseDate "2016-5-11"
        , endDate = parseDate "2016-5-12"
        , location = "Verona, Italy"
        , tags = [English, Developers, Italy, JavaScript]
        }
      , { name = "CSSConf Budapest"
        , link = "http://cssconfbp.rocks/"
        , startDate = parseDate "2016-5-11"
        , endDate = parseDate "2016-5-11"
        , location = "Budapest, Hungary"
        , tags = [English, Designers, Hungary, CSS]
        }
      , { name = "ApacheCon Core North America"
        , link = "http://www.apachecon.com/"
        , startDate = parseDate "2016-5-12"
        , endDate = parseDate "2016-5-13"
        , location = "Vancouver, Canada"
        , tags = [English, Developers, Canada, OpenSource]
        }
      , { name = "JSConf Budapest"
        , link = "http://jsconfbp.com/"
        , startDate = parseDate "2016-5-12"
        , endDate = parseDate "2016-5-13"
        , location = "Budapest, Hungary"
        , tags = [English, Developers, Hungary, JavaScript]
        }
      , { name = "An Event Apart Boston"
        , link = "http://aneventapart.com/event/boston-2016"
        , startDate = parseDate "2016-5-16"
        , endDate = parseDate "2016-5-18"
        , location = "Boston, MA"
        , tags = [English, Designers, Developers, USA, UX]
        }
      , { name = "Open Source Convention Tutorials"
        , link = "http://conferences.oreilly.com/oscon/open-source"
        , startDate = parseDate "2016-5-16"
        , endDate = parseDate "2016-5-17"
        , location = "Austin, TX"
        , tags = [English, Developers, USA, OpenSource]
        }
      , { name = "Front-Trends"
        , link = "http://2016.front-trends.com/"
        , startDate = parseDate "2016-5-18"
        , endDate = parseDate "2016-5-20"
        , location = "Warsaw, Poland"
        , tags = [English, Designers, Poland, UX]
        }
      , { name = "Open Source Convention"
        , link = "http://conferences.oreilly.com/oscon/open-source"
        , startDate = parseDate "2016-5-18"
        , endDate = parseDate "2016-5-19"
        , location = "Austin, TX"
        , tags = [English, Developers, USA, OpenSource]
        }
      , { name = "PhoneGap Day EU"
        , link = "http://pgday.phonegap.com/eu2016/"
        , startDate = parseDate "2016-5-19"
        , endDate = parseDate "2016-5-20"
        , location = "Amsterdam, Netherlands"
        , tags = [English, Developers, Netherlands, PhoneGap, Mobile]
        }
      , { name = "self.conference"
        , link = "http://selfconference.org/"
        , startDate = parseDate "2016-5-20"
        , endDate = parseDate "2016-5-21"
        , location = "Detroit, MI"
        , tags = [English, Designers, Developers, USA, General, SoftSkills]
        }
      , { name = "UIKonf"
        , link = "http://www.uikonf.com/"
        , startDate = parseDate "2016-5-22"
        , endDate = parseDate "2016-5-25"
        , location = "Berlin, Germany"
        , tags = [English, Developers, Germany, IOS]
        }
      , { name = "GOTO Chicago Workshops"
        , link = "http://gotocon.com/chicago-2016"
        , startDate = parseDate "2016-5-23"
        , endDate = parseDate "2016-5-23"
        , location = "Chicago, IL"
        , tags = [English, Developers, USA, General]
        }
      , { name = "GOTO Chicago"
        , link = "http://gotocon.com/chicago-2016"
        , startDate = parseDate "2016-5-24"
        , endDate = parseDate "2016-5-25"
        , location = "Chicago, IL"
        , tags = [English, Developers, USA, General]
        }
      , { name = "SIGNAL 2016"
        , link = "https://www.twilio.com/signal"
        , startDate = parseDate "2016-5-24"
        , endDate = parseDate "2016-5-25"
        , location = "San Francisco, CA"
        , tags = [English, Developers, USA, Communications]
        }
      , { name = "UXLx"
        , link = "https://www.ux-lx.com/"
        , startDate = parseDate "2016-5-24"
        , endDate = parseDate "2016-5-27"
        , location = "Lisbon, Portugal"
        , tags = [English, Designers, Portugal, UX]
        }
      , { name = "GlueCon"
        , link = "http://gluecon.com/"
        , startDate = parseDate "2016-5-25"
        , endDate = parseDate "2016-5-26"
        , location = "Broomfield, CO"
        , tags = [English, Developers, USA, General, DevOps, BigData]
        }
      , { name = "PureScript Conf"
        , link = "https://twitter.com/lambda_conf/status/667099897984712704"
        , startDate = parseDate "2016-5-25"
        , endDate = parseDate "2016-5-25"
        , location = "Boulder, CO"
        , tags = [English, Developers, USA, FunctionalProgramming, PureScript]
        }
      , { name = "GOTO Chicago Workshops"
        , link = "http://gotocon.com/chicago-2016"
        , startDate = parseDate "2016-5-26"
        , endDate = parseDate "2016-5-26"
        , location = "Chicago, IL"
        , tags = [English, Developers, USA, General]
        }
      , { name = "LambdaConf"
        , link = "http://lambdaconf.us/"
        , startDate = parseDate "2016-5-26"
        , endDate = parseDate "2016-5-29"
        , location = "Boulder, CO"
        , tags = [English, Developers, USA, FunctionalProgramming, Haskell, Scala, PureScript]
        }
      , { name = "Frontend United"
        , link = "http://frontendunited.org/"
        , startDate = parseDate "2016-5-27"
        , endDate = parseDate "2016-5-28"
        , location = "Ghent, Belgium"
        , tags = [English, Designers, Developers, Belgium, UX, Drupal]
        }
      , { name = "PyCon Tutorials"
        , link = "https://us.pycon.org/2016/"
        , startDate = parseDate "2016-5-28"
        , endDate = parseDate "2016-5-29"
        , location = "Portland, OR"
        , tags = [English, Developers, USA, Python]
        }
      , { name = "PyCon"
        , link = "https://us.pycon.org/2016/"
        , startDate = parseDate "2016-5-30"
        , endDate = parseDate "2016-6-1"
        , location = "Portland, OR"
        , tags = [English, Developers, USA, Python]
        }
      , { name = "MagmaConf"
        , link = "http://www.magmaconf.com/"
        , startDate = parseDate "2016-6-1"
        , endDate = parseDate "2016-6-3"
        , location = "Manzanillo, Mexico"
        , tags = [English, Developers, Mexico, Ruby]
        }
      , { name = "CSSConf Nordic"
        , link = "http://cssconf.no/"
        , startDate = parseDate "2016-6-1"
        , endDate = parseDate "2016-6-1"
        , location = "Oslo, Norway"
        , tags = [English, Designers, Norway, UX, CSS]
        }
      , { name = "GR8Conf EU"
        , link = "http://gr8conf.eu/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-4"
        , location = "Copenhagen, Denmark"
        , tags = [English, Developers, Denmark, Java, Groovy, Grails, Gradle]
        }
      , { name = "PyCon Sprints"
        , link = "https://us.pycon.org/2016/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-5"
        , location = "Portland, OR"
        , tags = [English, Developers, USA, Python]
        }
      , { name = "SoCraTes UK"
        , link = "http://socratesuk.org/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-5"
        , location = "Worcestershire, UK"
        , tags = [English, Developers, UK, SoftwareCraftsmanship]
        }
      , { name = "ReactEurope"
        , link = "https://www.react-europe.org/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-3"
        , location = "Paris, France"
        , tags = [English, Developers, France, React, JavaScript]
        }
      , { name = "Scotland JS"
        , link = "http://scotlandjs.com/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-3"
        , location = "Edinburgh, Scotland"
        , tags = [English, Developers, Scotland, JavaScript]
        }
      , { name = "Web Rebels"
        , link = "https://www.webrebels.org/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-3"
        , location = "Oslo, Norway"
        , tags = [English, Developers, Norway, JavaScript]
        }
      , { name = "Berlin Buzzwords"
        , link = "http://berlinbuzzwords.de/"
        , startDate = parseDate "2016-6-5"
        , endDate = parseDate "2016-6-7"
        , location = "Berlin, Germany"
        , tags = [English, Developers, Germany, BigData]
        }
      , { name = "NDC Oslo Workshops"
        , link = "http://ndcoslo.com/"
        , startDate = parseDate "2016-6-6"
        , endDate = parseDate "2016-6-7"
        , location = "Oslo, Norway"
        , tags = [English, Developers, Norway, Agile, DotNet, General]
        }
      , { name = "NDC Oslo"
        , link = "http://ndcoslo.com/"
        , startDate = parseDate "2016-6-8"
        , endDate = parseDate "2016-6-10"
        , location = "Oslo, Norway"
        , tags = [English, Developers, Norway, Agile, DotNet, General]
        }
      , { name = "UX Scotland"
        , link = "http://uxscotland.net/2016/"
        , startDate = parseDate "2016-6-8"
        , endDate = parseDate "2016-6-10"
        , location = "Edinburgh, Scotland"
        , tags = [English, Designers, Scotland, UX]
        }
      , { name = "QCon New York"
        , link = "https://qconnewyork.com/"
        , startDate = parseDate "2016-6-13"
        , endDate = parseDate "2016-6-15"
        , location = "New York, NY"
        , tags = [English, Developers, USA, General]
        }
      , { name = "GOTO Amsterdam Workshops"
        , link = "http://gotocon.com/amsterdam-2016/"
        , startDate = parseDate "2016-6-13"
        , endDate = parseDate "2016-6-13"
        , location = "Amsterdam, Netherlands"
        , tags = [English, Developers, Netherlands, General]
        }
      , { name = "GOTO Amsterdam"
        , link = "http://gotocon.com/amsterdam-2016/"
        , startDate = parseDate "2016-6-14"
        , endDate = parseDate "2016-6-15"
        , location = "Amsterdam, Netherlands"
        , tags = [English, Developers, Netherlands, General]
        }
      , { name = "Front End Design Conference"
        , link = "http://frontenddesignconference.com/"
        , startDate = parseDate "2016-6-15"
        , endDate = parseDate "2016-6-17"
        , location = "St. Petersburg, FL"
        , tags = [English, Designers, USA, UX]
        }
      , { name = "CSS Day"
        , link = "http://cssday.nl/2016"
        , startDate = parseDate "2016-6-16"
        , endDate = parseDate "2016-6-17"
        , location = "Amsterdam, Netherlands"
        , tags = [English, Designers, Netherlands, CSS]
        }
      , { name = "QCon New York Tutorials"
        , link = "https://qconnewyork.com/"
        , startDate = parseDate "2016-6-16"
        , endDate = parseDate "2016-6-17"
        , location = "New York, NY"
        , tags = [English, Developers, USA, General]
        }
      , { name = "DockerCon"
        , link = "http://2016.dockercon.com/"
        , startDate = parseDate "2016-6-19"
        , endDate = parseDate "2016-6-21"
        , location = "Seattle, WA"
        , tags = [English, Developers, USA, Docker]
        }
      , { name = "O'Reilly Velocity"
        , link = "http://conferences.oreilly.com/velocity/devops-web-performance-ca/"
        , startDate = parseDate "2016-6-21"
        , endDate = parseDate "2016-6-23"
        , location = "Santa Clara, CA"
        , tags = [English, Developers, USA, Scalability, DevOps]
        }
      , { name = "Web Design Day"
        , link = "http://webdesignday.com/"
        , startDate = parseDate "2016-6-23"
        , endDate = parseDate "2016-6-24"
        , location = "Pittsburgh, PA"
        , tags = [English, Designers, USA, UX]
        }
      , { name = "Dinosaur.js"
        , link = "http://dinosaurjs.org/"
        , startDate = parseDate "2016-6-24"
        , endDate = parseDate "2016-6-24"
        , location = "Denver, CO"
        , tags = [English, Developers, USA, JavaScript]
        }
      , { name = "GIANT Conf"
        , link = "http://www.giantux.com/conf2016/"
        , startDate = parseDate "2016-6-27"
        , endDate = parseDate "2016-6-29"
        , location = "TBD"
        , tags = [English, Designers, USA, UX]
        }
      , { name = "Hadoop Summit North America"
        , link = "http://2016.hadoopsummit.org/san-jose/"
        , startDate = parseDate "2016-6-28"
        , endDate = parseDate "2016-6-30"
        , location = "San Jose, CA"
        , tags = [English, Developers, USA, Hadoop, BigData, InternetOfThings]
        }
      , { name = "MongoDB World 2016"
        , link = "https://www.mongodb.com/world16"
        , startDate = parseDate "2016-6-28"
        , endDate = parseDate "2016-6-29"
        , location = "New York, NY"
        , tags = [English, Developers, USA, MongoDB, BigData]
        }
      , { name = "Brighton Ruby"
        , link = "http://brightonruby.com/"
        , startDate = parseDate "2016-7-8"
        , endDate = parseDate "2016-7-8"
        , location = "Brighton, UK"
        , tags = [English, Developers, UK, Ruby]
        }
      , { name = "Chef Conf"
        , link = "https://www.chef.io/chefconf/"
        , startDate = parseDate "2016-7-11"
        , endDate = parseDate "2016-7-13"
        , location = "Austin, TX"
        , tags = [English, Developers, USA, Chef, DevOps]
        }
      , { name = "GopherCon"
        , link = "https://www.gophercon.com/"
        , startDate = parseDate "2016-7-11"
        , endDate = parseDate "2016-7-13"
        , location = "Denver, CO"
        , tags = [English, Developers, USA, Go]
        }
      , { name = "EuroPython"
        , link = "http://ep2016.europython.eu/"
        , startDate = parseDate "2016-7-17"
        , endDate = parseDate "2016-7-24"
        , location = "Bilbao, Spain"
        , tags = [English, Developers, Spain, Python]
        }
      , { name = "An Event Apart DC"
        , link = "http://aneventapart.com/event/washington-dc-2016"
        , startDate = parseDate "2016-7-25"
        , endDate = parseDate "2016-7-27"
        , location = "Washington, DC"
        , tags = [English, Designers, Developers, USA, UX]
        }
      , { name = "NDC Sydney Workshops"
        , link = "http://ndcsydney.com/"
        , startDate = parseDate "2016-8-1"
        , endDate = parseDate "2016-8-2"
        , location = "Sydney, Australia"
        , tags = [English, Developers, Australia, Agile, DotNet, General]
        }
      , { name = "NDC Sydney"
        , link = "http://ndcsydney.com/"
        , startDate = parseDate "2016-8-3"
        , endDate = parseDate "2016-8-5"
        , location = "Sydney, Australia"
        , tags = [English, Developers, Australia, Agile, DotNet, General]
        }
      , { name = "That Conference"
        , link = "https://www.thatconference.com/"
        , startDate = parseDate "2016-8-8"
        , endDate = parseDate "2016-8-10"
        , location = "Wisconsin Dells, WI"
        , tags = [English, Developers, USA, Mobile, Cloud]
        }
      , { name = "FP Conf"
        , link = "http://fpconf.org/"
        , startDate = parseDate "2016-8-15"
        , endDate = parseDate "2016-8-15"
        , location = "Moscow, Russia"
        , tags = [English, Russian, Developers, Russia, FunctionalProgramming, Erlang, Scala, Clojure, Haskell]
        }
      , { name = "360|iDev"
        , link = "http://360idev.com/"
        , startDate = parseDate "2016-8-21"
        , endDate = parseDate "2016-8-24"
        , location = "Denver, CO"
        , tags = [English, Developers, USA, IOS]
        }
      , { name = "React Rally"
        , link = "http://www.reactrally.com/"
        , startDate = parseDate "2016-8-25"
        , endDate = parseDate "2016-8-26"
        , location = "Salt Lake City, UT"
        , tags = [English, Developers, USA, React, JavaScript]
        }
      , { name = "AlterConf South Africa"
        , link = "http://www.alterconf.com/sessions/cape-town-south-africa"
        , startDate = parseDate "2016-8-27"
        , endDate = parseDate "2016-8-27"
        , location = "Cape Town, South Africa"
        , tags = [English, Designers, Developers, SouthAfrica, Diversity, SoftSkills]
        }
      , { name = "An Event Apart Chicago"
        , link = "http://aneventapart.com/event/chicago-2016"
        , startDate = parseDate "2016-8-29"
        , endDate = parseDate "2016-8-31"
        , location = "Chicago, IL"
        , tags = [English, Designers, Developers, USA, UX]
        }
      , { name = "Frontend Conference Zurich"
        , link = "https://frontendconf.ch/"
        , startDate = parseDate "2016-9-1"
        , endDate = parseDate "2016-9-2"
        , location = "Zürich, Switzerland"
        , tags = [English, Designers, Switzerland, UX]
        }
      , { name = "Full Stack Fest"
        , link = "http://2016.fullstackfest.com/"
        , startDate = parseDate "2016-9-5"
        , endDate = parseDate "2016-9-9"
        , location = "Barcelona, Spain"
        , tags = [English, Developers, Spain, General]
        }
      , { name = "CocoaConf DC"
        , link = "http://cocoaconf.com/dc-2016/home"
        , startDate = parseDate "2016-9-9"
        , endDate = parseDate "2016-9-10"
        , location = "Washington, DC"
        , tags = [English, Developers, USA, Cocoa, IOS]
        }
      , { name = "SwanseaCon"
        , link = "http://swanseacon.co.uk/"
        , startDate = parseDate "2016-9-12"
        , endDate = parseDate "2016-9-13"
        , location = "Swansea, UK"
        , tags = [English, Developers, UK, Agile, SoftwareCraftsmanship]
        }
      , { name = "From the Front"
        , link = "http://2016.fromthefront.it/"
        , startDate = parseDate "2016-9-15"
        , endDate = parseDate "2016-9-16"
        , location = "Bologna, Italy"
        , tags = [English, Designers, Italy, UX]
        }
      , { name = "Strangeloop"
        , link = "http://thestrangeloop.com/"
        , startDate = parseDate "2016-9-15"
        , endDate = parseDate "2016-9-17"
        , location = "St. Louis, MO"
        , tags = [English, Developers, USA, General, FunctionalProgramming]
        }
      , { name = "WindyCityRails"
        , link = "https://www.windycityrails.org/"
        , startDate = parseDate "2016-9-15"
        , endDate = parseDate "2016-9-16"
        , location = "Chicago, IL"
        , tags = [English, Developers, USA, Ruby, Rails]
        }
      , { name = "International Conference on Functional Programming"
        , link = "http://conf.researchr.org/home/icfp-2016"
        , startDate = parseDate "2016-9-18"
        , endDate = parseDate "2016-9-24"
        , location = "Nara, Japan"
        , tags = [English, Developers, Japan, FunctionalProgramming, Haskell]
        }
      , { name = "CppCon"
        , link = "http://cppcon.org/"
        , startDate = parseDate "2016-9-18"
        , endDate = parseDate "2016-9-23"
        , location = "Bellevue, WA"
        , tags = [English, Developers, USA, CPlusPlus]
        }
      , { name = "Functional Conf"
        , link = "http://functionalconf.com/"
        , startDate = parseDate "2016-9-22"
        , endDate = parseDate "2016-9-25"
        , location = "Bangalore, India"
        , tags = [English, Developers, India, FunctionalProgramming]
        }
      , { name = "GOTO Copenhagen"
        , link = "http://gotocon.com/cph-2015/"
        , startDate = parseDate "2016-10-3"
        , endDate = parseDate "2016-10-6"
        , location = "Copenhagen, Denmark"
        , tags = [English, Developers, Denmark, General]
        }
      , { name = "An Event Apart Orlando"
        , link = "http://aneventapart.com/event/orlando-special-edition-2016"
        , startDate = parseDate "2016-10-3"
        , endDate = parseDate "2016-10-5"
        , location = "Orlando, FL"
        , tags = [English, Designers, Developers, USA, UX]
        }
      , { name = "dotGo"
        , link = "http://2016.dotgo.eu/"
        , startDate = parseDate "2016-10-10"
        , endDate = parseDate "2016-10-10"
        , location = "Paris, France"
        , tags = [English, Developers, France, Go]
        }
      , { name = "GOTO London"
        , link = "http://gotocon.com/"
        , startDate = parseDate "2016-10-12"
        , endDate = parseDate "2016-10-14"
        , location = "London, UK"
        , tags = [English, Developers, UK, General]
        }
      , { name = "ConnectJS"
        , link = "http://connect-js.com/"
        , startDate = parseDate "2016-10-21"
        , endDate = parseDate "2016-10-22"
        , location = "Atlanta, GA"
        , tags = [English, Developers, USA, JavaScript]
        }
      , { name = "Smashing Conf Barcelona"
        , link = "http://smashingconf.com/"
        , startDate = parseDate "2016-10-25"
        , endDate = parseDate "2016-10-26"
        , location = "Barcelona, Spain"
        , tags = [English, Designers, Spain, UX]
        }
      , { name = "An Event Apart San Francisco"
        , link = "http://aneventapart.com/event/san-francisco-2016"
        , startDate = parseDate "2016-10-31"
        , endDate = parseDate "2016-11-2"
        , location = "San Francisco, CA"
        , tags = [English, Designers, Developers, USA, UX]
        }
      , { name = "Beyond Tellerrand"
        , link = "http://beyondtellerrand.com/events/berlin-2016"
        , startDate = parseDate "2016-11-7"
        , endDate = parseDate "2016-11-9"
        , location = "Berlin, Germany"
        , tags = [English, Designers, Developers, Germany, UX, General]
        }
      , { name = "RubyConf"
        , link = "http://rubyconf.org/"
        , startDate = parseDate "2016-11-10"
        , endDate = parseDate "2016-11-12"
        , location = "Cincinnati, OH"
        , tags = [English, Developers, USA, Ruby]
        }
      , { name = "BuildStuff"
        , link = "http://buildstuff.lt/"
        , startDate = parseDate "2016-11-16"
        , endDate = parseDate "2016-11-20"
        , location = "Vilnius, Lithuania"
        , tags = [English, Developers, Lithuania, General]
        }
      , { name = "AWS re:Invent"
        , link = "https://reinvent.awsevents.com/"
        , startDate = parseDate "2016-11-28"
        , endDate = parseDate "2016-12-2"
        , location = "Las Vegas, NV"
        , tags = [English, Developers, USA, AWS, Cloud]
        }
      ]
  , tags =
    [ ("Conference Language",
      [
        (Excluded English, "English")
      , (Excluded French, "French")
      , (Excluded German, "German")
      , (Excluded Italian, "Italian")
      , (Excluded Russian, "Russian")
      ])
    , ("Audience",
      [
        (Excluded Designers, "Designers")
      , (Excluded Developers, "Developers")
      ])
    , ("Programming Languages/Technologies",
      [
        (Excluded Android, "Android")
      , (Excluded AngularJS, "AngularJS")
      , (Excluded AWS, "AWS")
      , (Excluded CPlusPlus, "C++")
      , (Excluded CSS, "CSS")
      , (Excluded Chef, "Chef")
      , (Excluded Clojure, "Clojure")
      , (Excluded Cocoa, "Cocoa")
      , (Excluded CycleJS, "CycleJS")
      , (Excluded Docker, "Docker")
      , (Excluded Drupal, "Drupal")
      , (Excluded DotNet, ".NET")
      , (Excluded Elasticserch, "Elasticserch")
      , (Excluded Ember, "Ember")
      , (Excluded Erlang, "Erlang")
      , (Excluded FSharp, "F#")
      , (Excluded Go, "Go")
      , (Excluded Gradle, "Gradle")
      , (Excluded Grails, "Grails")
      , (Excluded Groovy, "Groovy")
      , (Excluded Hadoop, "Hadoop")
      , (Excluded Haskell, "Haskell")
      , (Excluded IOS, "iOS")
      , (Excluded Java, "Java")
      , (Excluded JavaScript, "JavaScript")
      , (Excluded Logstash, "Logstash")
      , (Excluded MongoDB, "MongoDB")
      , (Excluded NodeJS, "NodeJS")
      , (Excluded OCaml, "OCaml")
      , (Excluded PhoneGap, "PhoneGap")
      , (Excluded PureScript, "PureScript")
      , (Excluded Python, "Python")
      , (Excluded Rails, "Rails")
      , (Excluded React, "React")
      , (Excluded Ruby, "Ruby")
      , (Excluded SML, "SML")
      , (Excluded Scala, "Scala")
      , (Excluded Swift, "Swift")
      ])
    , ("Topics",
      [
        (Excluded Agile, "Agile")
      , (Excluded BigData, "Big Data")
      , (Excluded Cloud, "Cloud")
      , (Excluded Communications, "Communications")
      , (Excluded DataVisualization, "DataVisualization")
      , (Excluded DevOps, "DevOps")
      , (Excluded Diversity, "Diversity")
      , (Excluded FunctionalProgramming, "Functional Programming")
      , (Excluded General, "General")
      , (Excluded InternetOfThings, "Internet of Things")
      , (Excluded Microservices, "Microservices")
      , (Excluded Mobile, "Mobile")
      , (Excluded NoSQL, "NoSQL")
      , (Excluded OpenSource, "Open Source")
      , (Excluded ProgressiveEnhancement, "Progressive Enhancement")
      , (Excluded Scalability, "Scalability")
      , (Excluded Security, "Security")
      , (Excluded SoftSkills, "Soft Skills")
      , (Excluded SoftwareCraftsmanship, "Software Craftsmanship")
      , (Excluded UX, "UX")
      ])
    , ("Locations",
      [
        (Excluded Australia, "Australia")
      , (Excluded Belarus, "Belarus")
      , (Excluded Belgium, "Belgium")
      , (Excluded Bulgaria, "Bulgaria")
      , (Excluded Canada, "Canada")
      , (Excluded China, "China")
      , (Excluded Denmark, "Denmark")
      , (Excluded France, "France")
      , (Excluded Germany, "Germany")
      , (Excluded Hungary, "Hungary")
      , (Excluded India, "India")
      , (Excluded Ireland, "Ireland")
      , (Excluded Italy, "Italy")
      , (Excluded Japan, "Japan")
      , (Excluded Lebanon, "Lebanon")
      , (Excluded Lithuania, "Lithuania")
      , (Excluded Mexico, "Mexico")
      , (Excluded Netherlands, "Netherlands")
      , (Excluded NewZealand, "New Zealand")
      , (Excluded Norway, "Norway")
      , (Excluded Philippines, "Philippines")
      , (Excluded Poland, "Poland")
      , (Excluded Portugal, "Portugal")
      , (Excluded Remote, "Remote")
      , (Excluded Romania, "Romania")
      , (Excluded Russia, "Russia")
      , (Excluded Scotland, "Scotland")
      , (Excluded SouthAfrica, "South Africa")
      , (Excluded Spain, "Spain")
      , (Excluded Sweden, "Sweden")
      , (Excluded Switzerland, "Switzerland")
      , (Excluded UAE, "UAE")
      , (Excluded Uruguay, "Uruguay")
      , (Excluded UK, "UK")
      , (Excluded USA, "USA")
      ]
    )]
  }
