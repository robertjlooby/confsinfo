module Conferences where

import Date
import DateFormatter exposing (parseDate)

type alias Model =
  { conferences : List Conference
  , tags : List (String, List FilteredTag)
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
  Agile
  | DotNet
  | London
  | Ruby

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

applyToAllTagsInList : (FilteredTag -> FilteredTag) -> List (String, List FilteredTag) -> List (String, List FilteredTag)
applyToAllTagsInList transform tagsWithDescription =
  List.map (\(description, tags) -> (description, List.map transform tags)) tagsWithDescription

excludeTag : Tag -> FilteredTag -> FilteredTag
excludeTag tag filteredTag =
  case filteredTag of
    Included t ->
      if t == tag then
        Excluded tag
      else
        filteredTag
    _ -> filteredTag

includeTag : Tag -> FilteredTag -> FilteredTag
includeTag tag filteredTag =
  case filteredTag of
    Excluded t ->
      if t == tag then
        Included tag
      else
        filteredTag
    _ -> filteredTag

excludeAllTags : FilteredTag -> FilteredTag
excludeAllTags tag =
  case tag of
    Excluded t -> Excluded t
    Included t -> Excluded t

shouldShow : List FilteredTag -> List Conference -> List Conference
shouldShow tags conferences =
  let
    filteredTagsToShow = List.filter shouldShowTag tags
    tagsToShow = List.map getTag filteredTagsToShow
  in
    List.filter (shouldShowConference tagsToShow) conferences

shouldShowConference : List Tag -> Conference -> Bool
shouldShowConference tags conference =
  List.all (\tag -> List.member tag conference.tags) tags

shouldShowTag : FilteredTag -> Bool
shouldShowTag tag =
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
        { name = "NDC London Workshops"
        , link = "http://ndc-london.com/"
        , startDate = parseDate "2016-1-11"
        , endDate = parseDate "2016-1-12"
        , location = "London, UK"
        , tags = [Agile, London]
        }
      , { name = "NDC London"
        , link = "http://ndc-london.com/"
        , startDate = parseDate "2016-1-13"
        , endDate = parseDate "2016-1-15"
        , location = "London, UK"
        , tags = [Agile, DotNet, London]
        }
      , { name = "/dev/winter"
        , link = "http://devcycles.net/2016/winter/"
        , startDate = parseDate "2016-1-23"
        , endDate = parseDate "2016-1-23"
        , location = "Cambridge, UK"
        , tags = []
        }
      , { name = "dotSwift"
        , link = "http://www.dotswift.io/"
        , startDate = parseDate "2016-1-29"
        , endDate = parseDate "2016-1-29"
        , location = "Paris, France"
        , tags = []
        }
      , { name = "FOSDEM"
        , link = "https://fosdem.org/2016/"
        , startDate = parseDate "2016-1-30"
        , endDate = parseDate "2016-1-31"
        , location = "Brussels, Belgium"
        , tags = []
        }
      , { name = "UX/DEV Summit"
        , link = "http://uxdsummit.com/"
        , startDate = parseDate "2016-2-4"
        , endDate = parseDate "2016-2-6"
        , location = "Fort Lauderdale, FL"
        , tags = []
        }
      , { name = "Compose 2016"
        , link = "http://www.composeconference.org/"
        , startDate = parseDate "2016-2-4"
        , endDate = parseDate "2016-2-5"
        , location = "Brooklyn, NY"
        , tags = []
        }
      , { name = "RubyFuza"
        , link = "http://www.rubyfuza.org/"
        , startDate = parseDate "2016-2-4"
        , endDate = parseDate "2016-2-5"
        , location = "Cape Town, South Africa"
        , tags = []
        }
      , { name = "The Microservices Conference"
        , link = "http://microxchg.io/2016/"
        , startDate = parseDate "2016-2-4"
        , endDate = parseDate "2016-2-5"
        , location = "Berlin, Germany"
        , tags = []
        }
      , { name = "Forward JS Workshops"
        , link = "http://forwardjs.com/home"
        , startDate = parseDate "2016-2-8"
        , endDate = parseDate "2016-2-9"
        , location = "San Francisco, CA"
        , tags = []
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
        , tags = []
        }
      , { name = "Forward JS Workshops"
        , link = "http://forwardjs.com/home"
        , startDate = parseDate "2016-2-11"
        , endDate = parseDate "2016-2-13"
        , location = "San Francisco, CA"
        , tags = []
        }
      , { name = "DevNexus"
        , link = "http://devnexus.com/s/index"
        , startDate = parseDate "2016-2-15"
        , endDate = parseDate "2016-2-17"
        , location = "Atlanta, GA"
        , tags = []
        }
      , { name = "Lambda Days"
        , link = "http://www.lambdadays.org/"
        , startDate = parseDate "2016-2-18"
        , endDate = parseDate "2016-2-19"
        , location = "Krakow, Poland"
        , tags = []
        }
      , { name = "BOB 2016"
        , link = "http://bobkonf.de/2016/en/"
        , startDate = parseDate "2016-2-19"
        , endDate = parseDate "2016-2-19"
        , location = "Berlin, Germany"
        , tags = []
        }
      , { name = ":clojureD"
        , link = "http://www.clojured.de/"
        , startDate = parseDate "2016-2-20"
        , endDate = parseDate "2016-2-20"
        , location = "Berlin, Germany"
        , tags = []
        }
      , { name = "React.js Conf"
        , link = "http://conf.reactjs.com/"
        , startDate = parseDate "2016-2-22"
        , endDate = parseDate "2016-2-23"
        , location = "San Francisco, CA"
        , tags = []
        }
      , { name = "ConFoo"
        , link = "http://confoo.ca/en"
        , startDate = parseDate "2016-2-24"
        , endDate = parseDate "2016-2-26"
        , location = "Montreal, QC, Canada"
        , tags = []
        }
      , { name = "Erlang Factory SF Workshops"
        , link = "http://www.erlang-factory.com/sfbay2016/home"
        , startDate = parseDate "2016-3-7"
        , endDate = parseDate "2016-3-9"
        , location = "San Francisco, CA"
        , tags = []
        }
      , { name = "QCon London"
        , link = "http://qconlondon.com/"
        , startDate = parseDate "2016-3-7"
        , endDate = parseDate "2016-3-9"
        , location = "London, UK"
        , tags = []
        }
      , { name = "jDays"
        , link = "http://www.jdays.se/"
        , startDate = parseDate "2016-3-8"
        , endDate = parseDate "2016-3-9"
        , location = "Gothenburg, Sweden"
        , tags = []
        }
      , { name = "SoCraTes ES"
        , link = "http://socrates-conference.es/doku.php"
        , startDate = parseDate "2016-3-10"
        , endDate = parseDate "2016-3-13"
        , location = "Gran Canaria, Spain"
        , tags = []
        }
      , { name = "Erlang Factory SF"
        , link = "http://www.erlang-factory.com/sfbay2016/home"
        , startDate = parseDate "2016-3-10"
        , endDate = parseDate "2016-3-11"
        , location = "San Francisco, CA"
        , tags = []
        }
      , { name = "QCon London Tutorials"
        , link = "http://qconlondon.com/"
        , startDate = parseDate "2016-3-10"
        , endDate = parseDate "2016-3-11"
        , location = "London, UK"
        , tags = []
        }
      , { name = "ScaleConf"
        , link = "http://scaleconf.org/"
        , startDate = parseDate "2016-3-10"
        , endDate = parseDate "2016-3-11"
        , location = "Cape Town, South Africa"
        , tags = []
        }
      , { name = "Bath Ruby"
        , link = "http://bathruby.us1.list-manage1.com/subscribe?u=f18bb7508370614edaffb50dd&id=73743da027"
        , startDate = parseDate "2016-3-11"
        , endDate = parseDate "2016-3-11"
        , location = "Bath, UK"
        , tags = []
        }
      , { name = "An Event Apart Nashville"
        , link = "http://aneventapart.com/event/nashville-2016"
        , startDate = parseDate "2016-3-14"
        , endDate = parseDate "2016-3-16"
        , location = "Nashville, TN"
        , tags = []
        }
      , { name = "Erlang Factory SF Workshops"
        , link = "http://www.erlang-factory.com/sfbay2016/home"
        , startDate = parseDate "2016-3-14"
        , endDate = parseDate "2016-3-16"
        , location = "San Francisco, CA"
        , tags = []
        }
      , { name = "Smashing Conf"
        , link = "http://smashingconf.com/"
        , startDate = parseDate "2016-3-15"
        , endDate = parseDate "2016-3-16"
        , location = "Oxford, UK"
        , tags = []
        }
      , { name = "Scale Summit"
        , link = "http://www.scalesummit.org/"
        , startDate = parseDate "2016-3-18"
        , endDate = parseDate "2016-3-18"
        , location = "London, UK"
        , tags = []
        }
      , { name = "EmberConf"
        , link = "http://emberconf.com/"
        , startDate = parseDate "2016-3-29"
        , endDate = parseDate "2016-3-30"
        , location = "Portland, OR"
        , tags = []
        }
      , { name = "Ruby on Ales"
        , link = "https://ruby.onales.com/"
        , startDate = parseDate "2016-3-31"
        , endDate = parseDate "2016-4-1"
        , location = "Bend, OR"
        , tags = []
        }
      , { name = "An Event Apart Seattle"
        , link = "http://aneventapart.com/event/seattle-2016"
        , startDate = parseDate "2016-4-4"
        , endDate = parseDate "2016-4-6"
        , location = "Seattle, WA"
        , tags = []
        }
      , { name = "Smashing Conf SF"
        , link = "http://smashingconf.com/sf-2016/"
        , startDate = parseDate "2016-4-5"
        , endDate = parseDate "2016-4-6"
        , location = "San Francisco, CA"
        , tags = []
        }
      , { name = "RubyConf Philippines"
        , link = "http://rubyconf.ph/"
        , startDate = parseDate "2016-4-7"
        , endDate = parseDate "2016-4-9"
        , location = "Taguig, Philippines"
        , tags = []
        }
      , { name = "Converge SE"
        , link = "http://convergese.com/"
        , startDate = parseDate "2016-4-13"
        , endDate = parseDate "2016-4-15"
        , location = "Columbia, SC"
        , tags = []
        }
      , { name = "Hadoop Summit Europe"
        , link = "http://2016.hadoopsummit.org/dublin/"
        , startDate = parseDate "2016-4-13"
        , endDate = parseDate "2016-4-14"
        , location = "Dublin, Ireland"
        , tags = []
        }
      , { name = "ACE! Conference"
        , link = "http://aceconf.com/"
        , startDate = parseDate "2016-4-14"
        , endDate = parseDate "2016-4-15"
        , location = "Krakow, Poland"
        , tags = []
        }
      , { name = "Scalar"
        , link = "http://scalar-conf.com/"
        , startDate = parseDate "2016-4-16"
        , endDate = parseDate "2016-4-16"
        , location = "Warsaw, Poland"
        , tags = []
        }
      , { name = "CycleConf"
        , link = "https://twitter.com/cycleconf"
        , startDate = parseDate "2016-4-21"
        , endDate = parseDate "2016-4-24"
        , location = "Copenhagen, Denmark"
        , tags = []
        }
      , { name = "Render Conf"
        , link = "http://2016.render-conf.com/"
        , startDate = parseDate "2016-4-21"
        , endDate = parseDate "2016-4-22"
        , location = "Oxford, UK"
        , tags = []
        }
      , { name = "dotSecurity"
        , link = "http://www.dotsecurity.io/"
        , startDate = parseDate "2016-4-22"
        , endDate = parseDate "2016-4-22"
        , location = "Paris, France"
        , tags = []
        }
      , { name = "Generate NY"
        , link = "http://generateconf.com/new-york-2016"
        , startDate = parseDate "2016-4-22"
        , endDate = parseDate "2016-4-22"
        , location = "New York, NY"
        , tags = []
        }
      , { name = "Future of Web Design"
        , link = "https://futureofwebdesign.com/london-2016/"
        , startDate = parseDate "2016-4-25"
        , endDate = parseDate "2016-4-27"
        , location = "London, UK"
        , tags = []
        }
      , { name = "dotScale"
        , link = "http://www.dotscale.io/"
        , startDate = parseDate "2016-4-25"
        , endDate = parseDate "2016-4-25"
        , location = "Paris, France"
        , tags = []
        }
      , { name = "Craft Conf"
        , link = "http://craft-conf.com/2016"
        , startDate = parseDate "2016-4-26"
        , endDate = parseDate "2016-4-29"
        , location = "Budapest, Hungary"
        , tags = []
        }
      , { name = "University of Illinois WebCon"
        , link = "http://webcon.illinois.edu/"
        , startDate = parseDate "2016-4-27"
        , endDate = parseDate "2016-4-28"
        , location = "Champaign, IL"
        , tags = []
        }
      , { name = "Future Insights Live"
        , link = "https://futureinsightslive.com/chicago-2016/"
        , startDate = parseDate "2016-5-2"
        , endDate = parseDate "2016-5-5"
        , location = "Chicago, IL"
        , tags = []
        }
      , { name = "ng-conf"
        , link = "http://www.ng-conf.org/"
        , startDate = parseDate "2016-5-4"
        , endDate = parseDate "2016-5-6"
        , location = "Salt Lake City, UT"
        , tags = []
        }
      , { name = "RailsConf"
        , link = "http://railsconf.com/"
        , startDate = parseDate "2016-5-4"
        , endDate = parseDate "2016-5-6"
        , location = "Kansas City, MO"
        , tags = []
        }
      , { name = "Apache: Big Data North America"
        , link = "http://www.apachecon.com/"
        , startDate = parseDate "2016-5-9"
        , endDate = parseDate "2016-5-11"
        , location = "Vancouver, BC, Canada"
        , tags = []
        }
      , { name = "Beyond Tellerrand"
        , link = "http://beyondtellerrand.com/events/duesseldorf-2016"
        , startDate = parseDate "2016-5-9"
        , endDate = parseDate "2016-5-11"
        , location = "Düsseldorf, Germany"
        , tags = []
        }
      , { name = "ChefConf"
        , link = "https://www.chef.io/chefconf/"
        , startDate = parseDate "2016-5-9"
        , endDate = parseDate "2016-5-11"
        , location = "Austin, TX"
        , tags = []
        }
      , { name = "jsDay"
        , link = "http://2016.jsday.it/"
        , startDate = parseDate "2016-5-11"
        , endDate = parseDate "2016-5-12"
        , location = "Verona, Italy"
        , tags = []
        }
      , { name = "CSSConf Budapest"
        , link = "http://cssconfbp.rocks/"
        , startDate = parseDate "2016-5-11"
        , endDate = parseDate "2016-5-11"
        , location = "Budapest, Hungary"
        , tags = []
        }
      , { name = "An Event Apart Boston"
        , link = "http://aneventapart.com/event/boston-2016"
        , startDate = parseDate "2016-5-16"
        , endDate = parseDate "2016-5-18"
        , location = "Boston, MA"
        , tags = []
        }
      , { name = "Open Source Convention Tutorials"
        , link = "http://conferences.oreilly.com/oscon/open-source"
        , startDate = parseDate "2016-5-16"
        , endDate = parseDate "2016-5-17"
        , location = "Austin, TX"
        , tags = []
        }
      , { name = "Front-Trends"
        , link = "http://2016.front-trends.com/"
        , startDate = parseDate "2016-5-18"
        , endDate = parseDate "2016-5-20"
        , location = "Warsaw, Poland"
        , tags = []
        }
      , { name = "Open Source Convention"
        , link = "http://conferences.oreilly.com/oscon/open-source"
        , startDate = parseDate "2016-5-18"
        , endDate = parseDate "2016-5-19"
        , location = "Austin, TX"
        , tags = []
        }
      , { name = "self.conference"
        , link = "http://selfconference.org/"
        , startDate = parseDate "2016-5-20"
        , endDate = parseDate "2016-5-21"
        , location = "Detroit, MI"
        , tags = []
        }
      , { name = "GOTO Chicago Workshops"
        , link = "http://gotocon.com/chicago-2016"
        , startDate = parseDate "2016-5-23"
        , endDate = parseDate "2016-5-23"
        , location = "Chicago, IL"
        , tags = []
        }
      , { name = "GOTO Chicago"
        , link = "http://gotocon.com/chicago-2016"
        , startDate = parseDate "2016-5-24"
        , endDate = parseDate "2016-5-25"
        , location = "Chicago, IL"
        , tags = []
        }
      , { name = "SIGNAL 2016"
        , link = "https://www.twilio.com/signal"
        , startDate = parseDate "2016-5-24"
        , endDate = parseDate "2016-5-25"
        , location = "San Francisco, CA"
        , tags = []
        }
      , { name = "GlueCon"
        , link = "http://gluecon.com/"
        , startDate = parseDate "2016-5-25"
        , endDate = parseDate "2016-5-26"
        , location = "Broomfield, CO"
        , tags = []
        }
      , { name = "GOTO Chicago Workshops"
        , link = "http://gotocon.com/chicago-2016"
        , startDate = parseDate "2016-5-26"
        , endDate = parseDate "2016-5-26"
        , location = "Chicago, IL"
        , tags = []
        }
      , { name = "PyCon Tutorials"
        , link = "https://us.pycon.org/2016/"
        , startDate = parseDate "2016-5-28"
        , endDate = parseDate "2016-5-29"
        , location = "Portland, OR"
        , tags = []
        }
      , { name = "PyCon"
        , link = "https://us.pycon.org/2016/"
        , startDate = parseDate "2016-5-30"
        , endDate = parseDate "2016-6-1"
        , location = "Portland, OR"
        , tags = []
        }
      , { name = "MagmaConf"
        , link = "http://www.magmaconf.com/"
        , startDate = parseDate "2016-6-1"
        , endDate = parseDate "2016-6-3"
        , location = "Manzanillo, Mexico"
        , tags = []
        }
      , { name = "CSSConf Nordic"
        , link = "http://cssconf.no/"
        , startDate = parseDate "2016-6-1"
        , endDate = parseDate "2016-6-1"
        , location = "Oslo, Norway"
        , tags = []
        }
      , { name = "PyCon Sprints"
        , link = "https://us.pycon.org/2016/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-5"
        , location = "Portland, OR"
        , tags = []
        }
      , { name = "SoCraTes UK"
        , link = "http://socratesuk.org/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-5"
        , location = "Worcestershire, UK"
        , tags = []
        }
      , { name = "ReactEurope"
        , link = "https://www.react-europe.org/"
        , startDate = parseDate "2016-6-2"
        , endDate = parseDate "2016-6-3"
        , location = "Paris, France"
        , tags = []
        }
      , { name = "Berlin Buzzwords"
        , link = "http://berlinbuzzwords.de/"
        , startDate = parseDate "2016-6-5"
        , endDate = parseDate "2016-6-7"
        , location = "Berlin, Germany"
        , tags = []
        }
      , { name = "NDC Oslo Workshops"
        , link = "http://ndcoslo.com/"
        , startDate = parseDate "2016-6-6"
        , endDate = parseDate "2016-6-7"
        , location = "Oslo, Norway"
        , tags = []
        }
      , { name = "NDC Oslo"
        , link = "http://ndcoslo.com/"
        , startDate = parseDate "2016-6-8"
        , endDate = parseDate "2016-6-10"
        , location = "Oslo, Norway"
        , tags = []
        }
      , { name = "QCon New York"
        , link = "https://qconnewyork.com/"
        , startDate = parseDate "2016-6-13"
        , endDate = parseDate "2016-6-15"
        , location = "New York, NY"
        , tags = []
        }
      , { name = "GOTO Amsterdam Workshops"
        , link = "http://gotocon.com/amsterdam-2016/"
        , startDate = parseDate "2016-6-13"
        , endDate = parseDate "2016-6-13"
        , location = "Amsterdam, Netherlands"
        , tags = []
        }
      , { name = "GOTO Amsterdam"
        , link = "http://gotocon.com/amsterdam-2016/"
        , startDate = parseDate "2016-6-14"
        , endDate = parseDate "2016-6-15"
        , location = "Amsterdam, Netherlands"
        , tags = []
        }
      , { name = "QCon New York Tutorials"
        , link = "https://qconnewyork.com/"
        , startDate = parseDate "2016-6-16"
        , endDate = parseDate "2016-6-17"
        , location = "New York, NY"
        , tags = []
        }
      , { name = "DockerCon"
        , link = "http://2016.dockercon.com/"
        , startDate = parseDate "2016-6-19"
        , endDate = parseDate "2016-6-21"
        , location = "Seattle, WA"
        , tags = []
        }
      , { name = "O'Reilly Velocity"
        , link = "http://conferences.oreilly.com/velocity/devops-web-performance-ca/"
        , startDate = parseDate "2016-6-21"
        , endDate = parseDate "2016-6-23"
        , location = "Santa Clara, CA"
        , tags = []
        }
      , { name = "GIANT Conf"
        , link = "http://www.giantux.com/conf2016/"
        , startDate = parseDate "2016-6-27"
        , endDate = parseDate "2016-6-29"
        , location = "TBD"
        , tags = []
        }
      , { name = "Hadoop Summit North America"
        , link = "http://2016.hadoopsummit.org/san-jose/"
        , startDate = parseDate "2016-6-28"
        , endDate = parseDate "2016-6-30"
        , location = "San Jose, CA"
        , tags = []
        }
      , { name = "Brighton Ruby"
        , link = "http://brightonruby.com/"
        , startDate = parseDate "2016-7-8"
        , endDate = parseDate "2016-7-8"
        , location = "Brighton, UK"
        , tags = []
        }
      , { name = "Chef Conf"
        , link = "https://www.chef.io/chefconf/"
        , startDate = parseDate "2016-7-11"
        , endDate = parseDate "2016-7-13"
        , location = "Austin, TX"
        , tags = []
        }
      , { name = "EuroPython"
        , link = "http://ep2016.europython.eu/"
        , startDate = parseDate "2016-7-17"
        , endDate = parseDate "2016-7-24"
        , location = "Bilbao, Spain"
        , tags = []
        }
      , { name = "An Event Apart DC"
        , link = "http://aneventapart.com/event/washington-dc-2016"
        , startDate = parseDate "2016-7-25"
        , endDate = parseDate "2016-7-27"
        , location = "Washington, DC"
        , tags = []
        }
      , { name = "NDC Sydney Workshops"
        , link = "http://ndcsydney.com/"
        , startDate = parseDate "2016-8-1"
        , endDate = parseDate "2016-8-2"
        , location = "Sydney, Australia"
        , tags = []
        }
      , { name = "NDC Sydney"
        , link = "http://ndcsydney.com/"
        , startDate = parseDate "2016-8-3"
        , endDate = parseDate "2016-8-5"
        , location = "Sydney, Australia"
        , tags = []
        }
      , { name = "That Conference"
        , link = "https://www.thatconference.com/"
        , startDate = parseDate "2016-8-8"
        , endDate = parseDate "2016-8-10"
        , location = "Wisconsin Dells, WI"
        , tags = []
        }
      , { name = "FP Conf"
        , link = "http://fpconf.org/"
        , startDate = parseDate "2016-8-15"
        , endDate = parseDate "2016-8-15"
        , location = "Moscow, Russia"
        , tags = []
        }
      , { name = "React Rally"
        , link = "http://www.reactrally.com/"
        , startDate = parseDate "2016-8-25"
        , endDate = parseDate "2016-8-26"
        , location = "Salt Lake City, UT"
        , tags = []
        }
      , { name = "An Event Apart Chicago"
        , link = "http://aneventapart.com/event/chicago-2016"
        , startDate = parseDate "2016-8-29"
        , endDate = parseDate "2016-8-31"
        , location = "Chicago, IL"
        , tags = []
        }
      , { name = "Frontend Conference Zurich"
        , link = "https://frontendconf.ch/"
        , startDate = parseDate "2016-9-1"
        , endDate = parseDate "2016-9-2"
        , location = "Zürich, Switzerland"
        , tags = []
        }
      , { name = "SwanseaCon"
        , link = "http://swanseacon.co.uk/"
        , startDate = parseDate "2016-9-12"
        , endDate = parseDate "2016-9-13"
        , location = "Swansea, UK"
        , tags = []
        }
      , { name = "Strangeloop"
        , link = "http://thestrangeloop.com/"
        , startDate = parseDate "2016-9-15"
        , endDate = parseDate "2016-9-17"
        , location = "St. Louis, MO"
        , tags = []
        }
      , { name = "WindyCityRails"
        , link = "https://www.windycityrails.org/"
        , startDate = parseDate "2016-9-15"
        , endDate = parseDate "2016-9-16"
        , location = "Chicago, IL"
        , tags = []
        }
      , { name = "International Conference on Functional Programming"
        , link = "http://conf.researchr.org/home/icfp-2016"
        , startDate = parseDate "2016-9-18"
        , endDate = parseDate "2016-9-24"
        , location = "Nara, Japan"
        , tags = []
        }
      , { name = "GOTO Copenhagen"
        , link = "http://gotocon.com/cph-2015/"
        , startDate = parseDate "2016-10-3"
        , endDate = parseDate "2016-10-6"
        , location = "Copenhagen, Denmark"
        , tags = []
        }
      , { name = "An Event Apart Orlando"
        , link = "http://aneventapart.com/event/orlando-special-edition-2016"
        , startDate = parseDate "2016-10-3"
        , endDate = parseDate "2016-10-5"
        , location = "Orlando, FL"
        , tags = []
        }
      , { name = "dotGo"
        , link = "http://2016.dotgo.eu/"
        , startDate = parseDate "2016-10-10"
        , endDate = parseDate "2016-10-10"
        , location = "Paris, France"
        , tags = []
        }
      , { name = "GOTO London"
        , link = "http://gotocon.com/"
        , startDate = parseDate "2016-10-12"
        , endDate = parseDate "2016-10-14"
        , location = "London, UK"
        , tags = []
        }
      , { name = "ConnectJS"
        , link = "http://connect-js.com/"
        , startDate = parseDate "2016-10-21"
        , endDate = parseDate "2016-10-22"
        , location = "Atlanta, GA"
        , tags = []
        }
      , { name = "Smashing Conf Barcelona"
        , link = "http://smashingconf.com/"
        , startDate = parseDate "2016-10-25"
        , endDate = parseDate "2016-10-26"
        , location = "Barcelona, Spain"
        , tags = []
        }
      , { name = "An Event Apart San Francisco"
        , link = "http://aneventapart.com/event/san-francisco-2016"
        , startDate = parseDate "2016-10-31"
        , endDate = parseDate "2016-11-2"
        , location = "San Francisco, CA"
        , tags = []
        }
      , { name = "Beyond Tellerrand"
        , link = "http://beyondtellerrand.com/events/berlin-2016"
        , startDate = parseDate "2016-11-7"
        , endDate = parseDate "2016-11-9"
        , location = "Berlin, Germany"
        , tags = []
        }
      , { name = "AWS re:Invent"
        , link = "https://reinvent.awsevents.com/"
        , startDate = parseDate "2016-11-28"
        , endDate = parseDate "2016-12-2"
        , location = "Las Vegas, NV"
        , tags = []
        }
      ]
  , tags =
    [("Languages/Technologies",
      [ Excluded Agile
      , Excluded DotNet
      , Excluded Ruby
      ])
    , ("Locations",
      [ Excluded London
      ]
    )]
  }
