module InitialData (model) where

import Conference
import Date exposing (Month(..))
import FilteredTag
import GenericSet
import Model
import Tag exposing (Tag(..))


model : Model.Model
model =
  { conferences =
      GenericSet.fromList
        Conference.compare'
        [ { name = "@Swift"
          , link = "http://atswift.io/index-en.html"
          , startDate = ( 2016, Jan, 10 )
          , endDate = ( 2016, Jan, 10 )
          , location = "Beijing, China"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, China, Swift, IOS ]
          }
        , { name = "NDC London Workshops"
          , link = "http://ndc-london.com/"
          , startDate = ( 2016, Jan, 11 )
          , endDate = ( 2016, Jan, 12 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, Agile, DotNet, General ]
          }
        , { name = "NDC London"
          , link = "http://ndc-london.com/"
          , startDate = ( 2016, Jan, 13 )
          , endDate = ( 2016, Jan, 15 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, Agile, DotNet, General ]
          }
        , { name = "Internet of Things Milan"
          , link = "https://www.mongodb.com/events/internet-of-things-milan"
          , startDate = ( 2016, Jan, 14 )
          , endDate = ( 2016, Jan, 14 )
          , location = "Milan, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Italy, MongoDB, BigData, InternetOfThings ]
          }
        , { name = "JS Remote Conf"
          , link = "https://allremoteconfs.com/js-2016"
          , startDate = ( 2016, Jan, 14 )
          , endDate = ( 2016, Jan, 16 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Remote, JavaScript ]
          }
        , { name = "GR8Conf IN"
          , link = "http://gr8conf.in/"
          , startDate = ( 2016, Jan, 16 )
          , endDate = ( 2016, Jan, 16 )
          , location = "New Delhi, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, India, Java, Groovy, Grails, Gradle ]
          }
        , { name = "O'Reilly Design Conference"
          , link = "http://conferences.oreilly.com/design/ux-interaction-iot-us"
          , startDate = ( 2016, Jan, 20 )
          , endDate = ( 2016, Jan, 22 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, UX ]
          }
        , { name = "SVG Summit"
          , link = "http://environmentsforhumans.com/2016/svg-summit/"
          , startDate = ( 2016, Jan, 21 )
          , endDate = ( 2016, Jan, 21 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, Remote, SVG ]
          }
        , { name = "/dev/winter"
          , link = "http://devcycles.net/2016/winter/"
          , startDate = ( 2016, Jan, 23 )
          , endDate = ( 2016, Jan, 23 )
          , location = "Cambridge, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, DevOps, NoSQL, FunctionalProgramming ]
          }
        , { name = "PhoneGap Day"
          , link = "http://pgday.phonegap.com/us2016/"
          , startDate = ( 2016, Jan, 28 )
          , endDate = ( 2016, Jan, 28 )
          , location = "Lehi, UT"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, PhoneGap, Mobile ]
          }
        , { name = "dotSwift"
          , link = "http://www.dotswift.io/"
          , startDate = ( 2016, Jan, 29 )
          , endDate = ( 2016, Jan, 29 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, France, Swift ]
          }
        , { name = "PHPBenelux"
          , link = "http://conference.phpbenelux.eu/2016/"
          , startDate = ( 2016, Jan, 29 )
          , endDate = ( 2016, Jan, 30 )
          , location = "Antwerp, Belgium"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Belgium, PHP ]
          }
        , { name = "AlterConf D.C."
          , link = "http://www.alterconf.com/sessions/washington-dc"
          , startDate = ( 2016, Jan, 30 )
          , endDate = ( 2016, Jan, 30 )
          , location = "Washington, DC"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, Diversity, SoftSkills ]
          }
        , { name = "FOSDEM"
          , link = "https://fosdem.org/2016/"
          , startDate = ( 2016, Jan, 30 )
          , endDate = ( 2016, Jan, 31 )
          , location = "Brussels, Belgium"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Belgium, OpenSource, General ]
          }
        , { name = "PgConf.Russia"
          , link = "https://pgconf.ru/"
          , startDate = ( 2016, Feb, 3 )
          , endDate = ( 2016, Feb, 5 )
          , location = "Moscow, Russia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Russian, Developers, Russia, PostgreSQL ]
          }
        , { name = "Compose 2016"
          , link = "http://www.composeconference.org/"
          , startDate = ( 2016, Feb, 4 )
          , endDate = ( 2016, Feb, 5 )
          , location = "Brooklyn, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, FunctionalProgramming, Haskell, FSharp, OCaml, SML ]
          }
        , { name = "RubyFuza"
          , link = "http://www.rubyfuza.org/"
          , startDate = ( 2016, Feb, 4 )
          , endDate = ( 2016, Feb, 5 )
          , location = "Cape Town, South Africa"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, SouthAfrica, Ruby ]
          }
        , { name = "SunshinePHP"
          , link = "http://2016.sunshinephp.com/"
          , startDate = ( 2016, Feb, 4 )
          , endDate = ( 2016, Feb, 6 )
          , location = "Miami, FL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, PHP ]
          }
        , { name = "The Microservices Conference"
          , link = "http://microxchg.io/2016/"
          , startDate = ( 2016, Feb, 4 )
          , endDate = ( 2016, Feb, 5 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Germany, Microservices ]
          }
        , { name = "UX/DEV Summit"
          , link = "http://uxdsummit.com/"
          , startDate = ( 2016, Feb, 4 )
          , endDate = ( 2016, Feb, 6 )
          , location = "Fort Lauderdale, FL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, UX, AngularJS, Ember, React ]
          }
        , { name = "Forward JS Workshops"
          , link = "http://forwardjs.com/home"
          , startDate = ( 2016, Feb, 8 )
          , endDate = ( 2016, Feb, 9 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, JavaScript, AngularJS, React, NodeJS ]
          }
        , { name = "Jfokus"
          , link = "http://www.jfokus.se/jfokus/"
          , startDate = ( 2016, Feb, 8 )
          , endDate = ( 2016, Feb, 10 )
          , location = "Stockholm, Sweden"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Sweden, Java, General ]
          }
        , { name = "Jfokus IoT"
          , link = "http://www.jfokus.se/iot/"
          , startDate = ( 2016, Feb, 8 )
          , endDate = ( 2016, Feb, 10 )
          , location = "Stockholm, Sweden"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Sweden, InternetOfThings ]
          }
        , { name = "Webstock"
          , link = "http://www.webstock.org.nz/16/"
          , startDate = ( 2016, Feb, 9 )
          , endDate = ( 2016, Feb, 12 )
          , location = "Wellington, New Zealand"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, NewZealand, General ]
          }
        , { name = "Forward JS"
          , link = "http://forwardjs.com/home"
          , startDate = ( 2016, Feb, 10 )
          , endDate = ( 2016, Feb, 10 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, JavaScript, AngularJS, React, NodeJS ]
          }
        , { name = "RubyConf Australia"
          , link = "http://www.rubyconf.org.au/2016"
          , startDate = ( 2016, Feb, 10 )
          , endDate = ( 2016, Feb, 13 )
          , location = "Gold Coast, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Australia, Ruby ]
          }
        , { name = "Clojure Remote"
          , link = "http://clojureremote.com/"
          , startDate = ( 2016, Feb, 11 )
          , endDate = ( 2016, Feb, 12 )
          , location = "Remote"
          , cfpStartDate = Just ( 2015, Oct, 30 )
          , cfpEndDate = Just ( 2015, Dec, 31 )
          , tags = [ English, Developers, Remote, Clojure, FunctionalProgramming ]
          }
        , { name = "Forward JS Workshops"
          , link = "http://forwardjs.com/home"
          , startDate = ( 2016, Feb, 11 )
          , endDate = ( 2016, Feb, 13 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, JavaScript, AngularJS, React, NodeJS ]
          }
        , { name = "DeveloperWeek"
          , link = "http://www.developerweek.com/"
          , startDate = ( 2016, Feb, 12 )
          , endDate = ( 2016, Feb, 18 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2015, Nov, 23 )
          , tags = [ English, Developers, USA, JavaScript, NodeJS, Python, PHP, DevOps, Ruby, Mobile, NoSQL, General ]
          }
        , { name = "DevNexus"
          , link = "http://devnexus.com/s/index"
          , startDate = ( 2016, Feb, 15 )
          , endDate = ( 2016, Feb, 17 )
          , location = "Atlanta, GA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Agile, BigData, Java, JavaScript, General ]
          }
        , { name = "Spark Summit East"
          , link = "https://spark-summit.org/east-2016/"
          , startDate = ( 2016, Feb, 16 )
          , endDate = ( 2016, Feb, 18 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, BigData ]
          }
        , { name = "Elastic{ON}"
          , link = "https://www.elastic.co/elasticon"
          , startDate = ( 2016, Feb, 17 )
          , endDate = ( 2016, Feb, 19 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2015, Nov, 30 )
          , tags = [ English, Developers, USA, MongoDB, BigData, Elasticsearch, Logstash ]
          }
        , { name = "DrupalCon Asia"
          , link = "https://events.drupal.org/asia2016"
          , startDate = ( 2016, Feb, 18 )
          , endDate = ( 2016, Feb, 21 )
          , location = "Mumbai, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2015, Nov, 2 )
          , tags = [ English, Developers, India, Drupal, PHP ]
          }
        , { name = "hello.js"
          , link = "http://hellojs.org/"
          , startDate = ( 2016, Feb, 18 )
          , endDate = ( 2016, Feb, 19 )
          , location = "Cluj-Napoca, Romania"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Romania, JavaScript ]
          }
        , { name = "Lambda Days"
          , link = "http://www.lambdadays.org/"
          , startDate = ( 2016, Feb, 18 )
          , endDate = ( 2016, Feb, 19 )
          , location = "Krakow, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Poland, FunctionalProgramming, Erlang ]
          }
        , { name = "BOB 2016"
          , link = "http://bobkonf.de/2016/en/"
          , startDate = ( 2016, Feb, 19 )
          , endDate = ( 2016, Feb, 19 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, German, Developers, Germany, FunctionalProgramming, Clojure, Haskell, Scala, Erlang ]
          }
        , { name = "GopherCon India"
          , link = "http://www.gophercon.in/"
          , startDate = ( 2016, Feb, 19 )
          , endDate = ( 2016, Feb, 10 )
          , location = "Bengaluru, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, India, Go ]
          }
        , { name = "Bulgaria Web Summit"
          , link = "http://bulgariawebsummit.com/"
          , startDate = ( 2016, Feb, 20 )
          , endDate = ( 2016, Feb, 20 )
          , location = "Sofia, Bulgaria"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, Bulgaria, UX, JavaScript, General ]
          }
        , { name = ":clojureD"
          , link = "http://www.clojured.de/"
          , startDate = ( 2016, Feb, 20 )
          , endDate = ( 2016, Feb, 20 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Germany, FunctionalProgramming, Clojure ]
          }
        , { name = "The Rolling Scopes Conference"
          , link = "http://2016.conf.rollingscopes.com/"
          , startDate = ( 2016, Feb, 20 )
          , endDate = ( 2016, Feb, 21 )
          , location = "Minsk, Belarus"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Russian, Developers, Belarus, CSS, NodeJS, JavaScript ]
          }
        , { name = "How.Camp"
          , link = "http://how.camp/"
          , startDate = ( 2016, Feb, 21 )
          , endDate = ( 2016, Feb, 21 )
          , location = "Sofia, Bulgaria"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, Bulgaria, UX, JavaScript ]
          }
        , { name = "React.js Conf"
          , link = "http://conf.reactjs.com/"
          , startDate = ( 2016, Feb, 22 )
          , endDate = ( 2016, Feb, 23 )
          , location = "San Francisco, CA"
          , cfpStartDate = Just ( 2015, Nov, 25 )
          , cfpEndDate = Just ( 2015, Dec, 13 )
          , tags = [ English, Developers, USA, React, JavaScript ]
          }
        , { name = "GopherCon Dubai"
          , link = "http://www.gophercon.ae/"
          , startDate = ( 2016, Feb, 23 )
          , endDate = ( 2016, Feb, 23 )
          , location = "Dubai, UAE"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, UAE, Go ]
          }
        , { name = "JavaScript Summit"
          , link = "http://environmentsforhumans.com/2016/javascript-summit/"
          , startDate = ( 2016, Feb, 23 )
          , endDate = ( 2016, Feb, 23 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Remote, JavaScript ]
          }
        , { name = "UXistanbul"
          , link = "http://uxistanbul.org/"
          , startDate = ( 2016, Feb, 23 )
          , endDate = ( 2016, Feb, 23 )
          , location = "Istanbul, Turkey"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Turkey, UX ]
          }
        , { name = "ConFoo"
          , link = "http://confoo.ca/en"
          , startDate = ( 2016, Feb, 24 )
          , endDate = ( 2016, Feb, 26 )
          , location = "Montreal, QC, Canada"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, French, Developers, Canada, General ]
          }
        , { name = "Freelance Remote Conf"
          , link = "https://allremoteconfs.com/freelance-2016"
          , startDate = ( 2016, Feb, 24 )
          , endDate = ( 2016, Feb, 26 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Designers, Remote, General, SoftSkills ]
          }
        , { name = "UX Riga"
          , link = "http://www.uxriga.lv/"
          , startDate = ( 2016, Feb, 25 )
          , endDate = ( 2016, Feb, 25 )
          , location = "Riga, Latvia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Latvia, UX ]
          }
        , { name = "Interaction 16"
          , link = "http://interaction16.ixda.org/"
          , startDate = ( 2016, Mar, 1 )
          , endDate = ( 2016, Mar, 4 )
          , location = "Helsinki, Finland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Finland, UX ]
          }
        , { name = "try! Swift"
          , link = "http://www.tryswiftconf.com/en"
          , startDate = ( 2016, Mar, 2 )
          , endDate = ( 2016, Mar, 4 )
          , location = "Tokyo, Japan"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Japan, Swift, IOS ]
          }
        , { name = "EnhanceConf"
          , link = "http://enhanceconf.com/"
          , startDate = ( 2016, Mar, 3 )
          , endDate = ( 2016, Mar, 4 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, CSS, JavaScript, ProgressiveEnhancement ]
          }
        , { name = "fsharpConf"
          , link = "http://fsharpconf.com/"
          , startDate = ( 2016, Mar, 4 )
          , endDate = ( 2016, Mar, 4 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Remote, FSharp, DotNet, FunctionalProgramming ]
          }
        , { name = "Chamberconf"
          , link = "http://chamberconf.pl/"
          , startDate = ( 2016, Mar, 5 )
          , endDate = ( 2016, Mar, 6 )
          , location = "Moszna, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ Polish, Developers, Poland, General ]
          }
        , { name = "droidcon Tunisia"
          , link = "http://www.droidcon.tn/"
          , startDate = ( 2016, Mar, 5 )
          , endDate = ( 2016, Mar, 6 )
          , location = "Hammamet, Tunisia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Tunisia, Android, Mobile ]
          }
        , { name = "Frontend Conference"
          , link = "http://kfug.jp/frontconf2016/"
          , startDate = ( 2016, Mar, 5 )
          , endDate = ( 2016, Mar, 5 )
          , location = "Osaka, Japan"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ Japanese, Designers, Developers, Japan, UX, JavaScript, CSS, AngularJS ]
          }
        , { name = "Big Data Paris"
          , link = "http://www.bigdataparis.com/"
          , startDate = ( 2016, Mar, 7 )
          , endDate = ( 2016, Mar, 8 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ French, Developers, France, MongoDB, BigData ]
          }
        , { name = "Erlang Factory SF Workshops"
          , link = "http://www.erlang-factory.com/sfbay2016/home"
          , startDate = ( 2016, Mar, 7 )
          , endDate = ( 2016, Mar, 9 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Erlang ]
          }
        , { name = "Fluent Conf Trainings"
          , link = "http://conferences.oreilly.com/fluent/javascript-html-us"
          , startDate = ( 2016, Mar, 7 )
          , endDate = ( 2016, Mar, 8 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, CSS, JavaScript, UX, React, AngularJS, Docker ]
          }
        , { name = "QCon London"
          , link = "http://qconlondon.com/"
          , startDate = ( 2016, Mar, 7 )
          , endDate = ( 2016, Mar, 9 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, General ]
          }
        , { name = "Fluent Conf"
          , link = "http://conferences.oreilly.com/fluent/javascript-html-us"
          , startDate = ( 2016, Mar, 8 )
          , endDate = ( 2016, Mar, 10 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, CSS, JavaScript, UX, React, AngularJS, Docker ]
          }
        , { name = "jDays"
          , link = "http://www.jdays.se/"
          , startDate = ( 2016, Mar, 8 )
          , endDate = ( 2016, Mar, 9 )
          , location = "Gothenburg, Sweden"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Sweden, Java ]
          }
        , { name = "Apache Geode Summit"
          , link = "http://geodesummit.com/"
          , startDate = ( 2016, Mar, 9 )
          , endDate = ( 2016, Mar, 9 )
          , location = "Palo Alto, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, BigData, Cloud ]
          }
        , { name = "Erlang Factory SF"
          , link = "http://www.erlang-factory.com/sfbay2016/home"
          , startDate = ( 2016, Mar, 10 )
          , endDate = ( 2016, Mar, 11 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Erlang ]
          }
        , { name = "ScaleConf"
          , link = "http://scaleconf.org/"
          , startDate = ( 2016, Mar, 10 )
          , endDate = ( 2016, Mar, 11 )
          , location = "Cape Town, South Africa"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, SouthAfrica, Scalability ]
          }
        , { name = "SoCraTes ES"
          , link = "http://www.socrates-conference.es/doku.php"
          , startDate = ( 2016, Mar, 10 )
          , endDate = ( 2016, Mar, 13 )
          , location = "Gran Canaria, Spain"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Spain, SoftwareCraftsmanship ]
          }
        , { name = "QCon London Tutorials"
          , link = "http://qconlondon.com/"
          , startDate = ( 2016, Mar, 10 )
          , endDate = ( 2016, Mar, 11 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, General ]
          }
        , { name = "Bath Ruby"
          , link = "http://bathruby.us1.list-manage1.com/subscribe?u=f18bb7508370614edaffb50dd&id=73743da027"
          , startDate = ( 2016, Mar, 11 )
          , endDate = ( 2016, Mar, 11 )
          , location = "Bath, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, Ruby ]
          }
        , { name = "RWDevCon 2016"
          , link = "http://rwdevcon.com/"
          , startDate = ( 2016, Mar, 11 )
          , endDate = ( 2016, Mar, 12 )
          , location = "Alexandria, VA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, IOS, Swift ]
          }
        , { name = "wroc_love.rb"
          , link = "http://www.wrocloverb.com/"
          , startDate = ( 2016, Mar, 11 )
          , endDate = ( 2016, Mar, 13 )
          , location = "Wroclaw, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Poland, Ruby ]
          }
        , { name = "JSConf Beirut"
          , link = "http://www.jsconfbeirut.com/"
          , startDate = ( 2016, Mar, 12 )
          , endDate = ( 2016, Mar, 13 )
          , location = "Beirut, Lebanon"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Lebanon, JavaScript ]
          }
        , { name = "An Event Apart Nashville"
          , link = "http://aneventapart.com/event/nashville-2016"
          , startDate = ( 2016, Mar, 14 )
          , endDate = ( 2016, Mar, 16 )
          , location = "Nashville, TN"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, UX ]
          }
        , { name = "CocoaConf Yosemite"
          , link = "http://cocoaconf.com/yosemite"
          , startDate = ( 2016, Mar, 14 )
          , endDate = ( 2016, Mar, 17 )
          , location = "National Park, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, IOS, Cocoa ]
          }
        , { name = "Erlang Factory SF Workshops"
          , link = "http://www.erlang-factory.com/sfbay2016/home"
          , startDate = ( 2016, Mar, 14 )
          , endDate = ( 2016, Mar, 16 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Erlang ]
          }
        , { name = "Smashing Conf"
          , link = "http://smashingconf.com/"
          , startDate = ( 2016, Mar, 15 )
          , endDate = ( 2016, Mar, 16 )
          , location = "Oxford, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, England, UX ]
          }
        , { name = "DIBI"
          , link = "http://dibiconference.com/"
          , startDate = ( 2016, Mar, 17 )
          , endDate = ( 2016, Mar, 18 )
          , location = "Edinburgh, Scotland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Scotland, UX ]
          }
        , { name = "droidcon San Francisco"
          , link = "http://sf.droidcon.com/"
          , startDate = ( 2016, Mar, 17 )
          , endDate = ( 2016, Mar, 18 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Android, Mobile ]
          }
        , { name = "mdevcon"
          , link = "http://mdevcon.com/"
          , startDate = ( 2016, Mar, 17 )
          , endDate = ( 2016, Mar, 18 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Just ( 2015, Nov, 8 )
          , cfpEndDate = Just ( 2015, Dec, 23 )
          , tags = [ English, Developers, Netherlands, Mobile, IOS, Android ]
          }
        , { name = "Nordic PGDay"
          , link = "http://2016.nordicpgday.org/"
          , startDate = ( 2016, Mar, 17 )
          , endDate = ( 2016, Mar, 17 )
          , location = "Helsinki, Finland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Finland, PostgreSQL ]
          }
        , { name = "pgDay Asia"
          , link = "http://2016.pgday.asia/"
          , startDate = ( 2016, Mar, 17 )
          , endDate = ( 2016, Mar, 19 )
          , location = "Singapore"
          , cfpStartDate = Just ( 2015, Nov, 26 )
          , cfpEndDate = Just ( 2016, Jan, 22 )
          , tags = [ English, Developers, Singapore, PostgreSQL ]
          }
        , { name = "Codemotion Rome"
          , link = "http://rome2016.codemotionworld.com/"
          , startDate = ( 2016, Mar, 18 )
          , endDate = ( 2016, Mar, 19 )
          , location = "Rome, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Italy, General ]
          }
        , { name = "Scale Summit"
          , link = "http://www.scalesummit.org/"
          , startDate = ( 2016, Mar, 18 )
          , endDate = ( 2016, Mar, 18 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, Scalability ]
          }
        , { name = "Dutch Clojure Days"
          , link = "http://clojure.org/events/2016/dutch_clojure_days"
          , startDate = ( 2016, Mar, 19 )
          , endDate = ( 2016, Mar, 19 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 23 )
          , tags = [ English, Developers, Netherlands, Clojure, FunctionalProgramming ]
          }
        , { name = "GR8Day Warsaw"
          , link = "http://warsaw.gr8days.pl/"
          , startDate = ( 2016, Mar, 19 )
          , endDate = ( 2016, Mar, 19 )
          , location = "Warsaw, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 1 )
          , tags = [ English, Developers, Poland, Java, Groovy, Grails, Gradle ]
          }
        , { name = "RubyConf India"
          , link = "http://rubyconfindia.org/"
          , startDate = ( 2016, Mar, 19 )
          , endDate = ( 2016, Mar, 20 )
          , location = "Kochi, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jan, 18 )
          , tags = [ English, Developers, India, Ruby ]
          }
        , { name = "SwiftAveiro"
          , link = "https://attending.io/events/swiftaveiro"
          , startDate = ( 2016, Mar, 20 )
          , endDate = ( 2016, Mar, 20 )
          , location = "Aveiro, Portugal"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Portugal, IOS, Swift ]
          }
        , { name = "MountainWest RubyConf"
          , link = "http://mtnwestrubyconf.org/2016/"
          , startDate = ( 2016, Mar, 21 )
          , endDate = ( 2016, Mar, 22 )
          , location = "Salt Lake City, UT"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Ruby ]
          }
        , { name = "PIPELINE"
          , link = "http://web.pipelineconf.info/"
          , startDate = ( 2016, Mar, 23 )
          , endDate = ( 2016, Mar, 23 )
          , location = "London, England"
          , cfpStartDate = Just ( 2015, Oct, 19 )
          , cfpEndDate = Just ( 2015, Dec, 18 )
          , tags = [ English, Developers, England, Agile ]
          }
        , { name = "Ruby Remote Conf"
          , link = "https://allremoteconfs.com/ruby-2016"
          , startDate = ( 2016, Mar, 23 )
          , endDate = ( 2016, Mar, 25 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 5 )
          , tags = [ English, Developers, Remote, Ruby ]
          }
        , { name = "CocoaConf Chicago"
          , link = "http://cocoaconf.com/chicago-2016/home"
          , startDate = ( 2016, Mar, 25 )
          , endDate = ( 2016, Mar, 26 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Cocoa, IOS ]
          }
        , { name = "CSS Day"
          , link = "http://2016.cssday.it/"
          , startDate = ( 2016, Mar, 25 )
          , endDate = ( 2016, Mar, 25 )
          , location = "Faenza, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ Italian, Designers, Italy, CSS ]
          }
        , { name = "DevExperience"
          , link = "http://devexperience.ro/"
          , startDate = ( 2016, Mar, 25 )
          , endDate = ( 2016, Mar, 25 )
          , location = "Lasi, Romania"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Romania, General ]
          }
        , { name = "droidcon Dubai"
          , link = "http://droidcon.ae/"
          , startDate = ( 2016, Mar, 25 )
          , endDate = ( 2016, Mar, 26 )
          , location = "Dubai, UAE"
          , cfpStartDate = Just ( 2015, Oct, 27 )
          , cfpEndDate = Just ( 2016, Feb, 28 )
          , tags = [ English, Developers, UAE, Android, Mobile ]
          }
        , { name = "Space City JS"
          , link = "http://spacecity.codes/"
          , startDate = ( 2016, Mar, 26 )
          , endDate = ( 2016, Mar, 26 )
          , location = "Houston, TX"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jan, 15 )
          , tags = [ English, Developers, USA, JavaScript ]
          }
        , { name = "EmberConf"
          , link = "http://emberconf.com/"
          , startDate = ( 2016, Mar, 29 )
          , endDate = ( 2016, Mar, 30 )
          , location = "Portland, OR"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Ember ]
          }
        , { name = "RWD Summit"
          , link = "http://environmentsforhumans.com/2016/responsive-web-design-summit/"
          , startDate = ( 2016, Mar, 29 )
          , endDate = ( 2016, Mar, 31 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Remote, CSS, UX ]
          }
        , { name = "Clarity Conf"
          , link = "http://clarityconf.com/"
          , startDate = ( 2016, Mar, 31 )
          , endDate = ( 2016, Apr, 1 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "Ruby on Ales"
          , link = "https://ruby.onales.com/"
          , startDate = ( 2016, Mar, 31 )
          , endDate = ( 2016, Apr, 1 )
          , location = "Bend, OR"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Ruby ]
          }
        , { name = "Codemotion Dubai"
          , link = "http://rome2016.codemotionworld.com/"
          , startDate = ( 2016, Apr, 1 )
          , endDate = ( 2016, Apr, 2 )
          , location = "Dubai, UAE"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, UAE, General ]
          }
        , { name = "Flourish!"
          , link = "http://flourishconf.com/2016/"
          , startDate = ( 2016, Apr, 1 )
          , endDate = ( 2016, Apr, 2 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, OpenSource ]
          }
        , { name = "Fronteers Spring Thing"
          , link = "https://fronteers.nl/spring"
          , startDate = ( 2016, Apr, 1 )
          , endDate = ( 2016, Apr, 1 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Netherlands, JavaScript ]
          }
        , { name = "An Event Apart Seattle"
          , link = "http://aneventapart.com/event/seattle-2016"
          , startDate = ( 2016, Apr, 4 )
          , endDate = ( 2016, Apr, 6 )
          , location = "Seattle, WA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, UX ]
          }
        , { name = "Istanbul Tech Talks"
          , link = "http://www.istanbultechtalks.com/"
          , startDate = ( 2016, Apr, 5 )
          , endDate = ( 2016, Apr, 5 )
          , location = "Istanbul, Turkey"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Turkey, General ]
          }
        , { name = "Smashing Conf SF"
          , link = "http://smashingconf.com/sf-2016/"
          , startDate = ( 2016, Apr, 5 )
          , endDate = ( 2016, Apr, 6 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "Ancient City Ruby"
          , link = "http://www.ancientcityruby.com/"
          , startDate = ( 2016, Apr, 6 )
          , endDate = ( 2016, Apr, 8 )
          , location = "St. Augustine, FL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Ruby ]
          }
        , { name = "AWS Summit Bogotá"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Apr, 6 )
          , endDate = ( 2016, Apr, 6 )
          , location = "Bogotá, Colombia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Colombia, AWS, DevOps ]
          }
        , { name = "droidcon Italy"
          , link = "http://it.droidcon.com/2016/"
          , startDate = ( 2016, Apr, 7 )
          , endDate = ( 2016, Apr, 8 )
          , location = "Turin, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jan, 31 )
          , tags = [ English, Developers, Italy, Android, Mobile ]
          }
        , { name = "Respond 2016 Sydney"
          , link = "http://www.webdirections.org/respond16/"
          , startDate = ( 2016, Apr, 7 )
          , endDate = ( 2016, Apr, 8 )
          , location = "Sydney, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Australia, CSS, UX ]
          }
        , { name = "RubyConf Philippines"
          , link = "http://rubyconf.ph/"
          , startDate = ( 2016, Apr, 7 )
          , endDate = ( 2016, Apr, 9 )
          , location = "Taguig, Philippines"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Philippines, Ruby ]
          }
        , { name = "Greach"
          , link = "http://greachconf.com/"
          , startDate = ( 2016, Apr, 8 )
          , endDate = ( 2016, Apr, 9 )
          , location = "Madrid, Spain"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Spain, Java, Groovy, Grails, Gradle ]
          }
        , { name = "Jazoon TechDays"
          , link = "http://jazoon.com/"
          , startDate = ( 2016, Apr, 8 )
          , endDate = ( 2016, Apr, 8 )
          , location = "Bern, Switzerland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Switzerland, JavaScript, AngularJS, Ember ]
          }
        , { name = "AlterConf Minneapolis"
          , link = "http://www.alterconf.com/sessions/minneapolis-mn"
          , startDate = ( 2016, Apr, 9 )
          , endDate = ( 2016, Apr, 9 )
          , location = "Minneapolis, MN"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 1 )
          , tags = [ English, Designers, Developers, USA, Diversity, SoftSkills ]
          }
        , { name = "MobCon"
          , link = "http://mobcon.com/mobcon-europe/"
          , startDate = ( 2016, Apr, 10 )
          , endDate = ( 2016, Apr, 10 )
          , location = "Sofia, Bulgaria"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 14 )
          , tags = [ English, Developers, Bulgaria, Mobile, IOS, Android ]
          }
        , { name = "Respond 2016"
          , link = "http://www.webdirections.org/respond16/"
          , startDate = ( 2016, Apr, 11 )
          , endDate = ( 2016, Apr, 12 )
          , location = "Melbourne, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Australia, CSS, UX ]
          }
        , { name = "Yggdrasil"
          , link = "http://yggdrasilkonferansen.no/"
          , startDate = ( 2016, Apr, 11 )
          , endDate = ( 2016, Apr, 12 )
          , location = "Sandefjord, Norway"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ Norwegian, Designers, Norway, UX ]
          }
        , { name = "AWS Summit Berlin"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Apr, 12 )
          , endDate = ( 2016, Apr, 12 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Germany, AWS, DevOps ]
          }
        , { name = "Converge SE"
          , link = "http://convergese.com/"
          , startDate = ( 2016, Apr, 13 )
          , endDate = ( 2016, Apr, 15 )
          , location = "Columbia, SC"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, General ]
          }
        , { name = "Hadoop Summit Europe"
          , link = "http://2016.hadoopsummit.org/dublin/"
          , startDate = ( 2016, Apr, 13 )
          , endDate = ( 2016, Apr, 14 )
          , location = "Dublin, Ireland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Ireland, Hadoop, BigData, InternetOfThings ]
          }
        , { name = "iOS Remote Conf"
          , link = "https://allremoteconfs.com/ios-2016"
          , startDate = ( 2016, Apr, 13 )
          , endDate = ( 2016, Apr, 15 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 12 )
          , tags = [ English, Developers, Remote, IOS ]
          }
        , { name = "Peers Conference"
          , link = "http://peersconf.com/"
          , startDate = ( 2016, Apr, 13 )
          , endDate = ( 2016, Apr, 15 )
          , location = "St. Petersburg, FL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, SoftSkills ]
          }
        , { name = "ACE! Conference"
          , link = "http://aceconf.com/"
          , startDate = ( 2016, Apr, 14 )
          , endDate = ( 2016, Apr, 15 )
          , location = "Krakow, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Poland, Agile, SoftwareCraftsmanship ]
          }
        , { name = "AWS Summit Milan"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Apr, 14 )
          , endDate = ( 2016, Apr, 14 )
          , location = "Milan, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Italy, AWS, DevOps ]
          }
        , { name = "Rootconf"
          , link = "https://rootconf.in/2016/"
          , startDate = ( 2016, Apr, 14 )
          , endDate = ( 2016, Apr, 15 )
          , location = "Bangalore, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 1 )
          , tags = [ English, Developers, India, DevOps, Cloud ]
          }
        , { name = "Clojure/west"
          , link = "http://clojurewest.org/"
          , startDate = ( 2016, Apr, 15 )
          , endDate = ( 2016, Apr, 16 )
          , location = "Seattle, WA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Clojure, FunctionalProgramming ]
          }
        , { name = "CocoaConf Austin"
          , link = "http://cocoaconf.com/austin-2016/home"
          , startDate = ( 2016, Apr, 15 )
          , endDate = ( 2016, Apr, 16 )
          , location = "Austin, TX"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Cocoa, IOS ]
          }
        , { name = "JSConf Uruguay"
          , link = "https://jsconf.uy/"
          , startDate = ( 2016, Apr, 15 )
          , endDate = ( 2016, Apr, 16 )
          , location = "Montevideo, Uruguay"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Uruguay, JavaScript ]
          }
        , { name = "Scalar"
          , link = "http://scalar-conf.com/"
          , startDate = ( 2016, Apr, 16 )
          , endDate = ( 2016, Apr, 16 )
          , location = "Warsaw, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Poland, Scala ]
          }
        , { name = "JavaScript Frameworks Day"
          , link = "http://frameworksdays.com/event/js-frameworks-day-2016"
          , startDate = ( 2016, Apr, 17 )
          , endDate = ( 2016, Apr, 17 )
          , location = "Kiev, Ukraine"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ Russian, Developers, Ukraine, JavaScript ]
          }
        , { name = "AWS Summit Chicago"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Apr, 18 )
          , endDate = ( 2016, Apr, 19 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, AWS, DevOps ]
          }
        , { name = "PGConf US"
          , link = "http://www.pgconf.us/2016/"
          , startDate = ( 2016, Apr, 18 )
          , endDate = ( 2016, Apr, 20 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jan, 31 )
          , tags = [ English, Developers, USA, PostgreSQL ]
          }
        , { name = "ACCU"
          , link = "http://accu.org/index.php/conferences/accu_conference_2016"
          , startDate = ( 2016, Apr, 19 )
          , endDate = ( 2016, Apr, 23 )
          , location = "Bristol, England"
          , cfpStartDate = Just ( 2015, Oct, 12 )
          , cfpEndDate = Just ( 2015, Nov, 13 )
          , tags = [ English, Developers, England, General, CPlusPlus ]
          }
        , { name = "Industry Conf"
          , link = "http://2016.industryconf.com/"
          , startDate = ( 2016, Apr, 20 )
          , endDate = ( 2016, Apr, 20 )
          , location = "Newcastle, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, General ]
          }
        , { name = "CycleConf"
          , link = "http://cycleconf.com/"
          , startDate = ( 2016, Apr, 21 )
          , endDate = ( 2016, Apr, 24 )
          , location = "Copenhagen, Denmark"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Denmark, CycleJS, JavaScript ]
          }
        , { name = "MCE"
          , link = "http://mceconf.com/"
          , startDate = ( 2016, Apr, 21 )
          , endDate = ( 2016, Apr, 22 )
          , location = "Warsaw, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 29 )
          , tags = [ English, Designers, Developers, Poland, Mobile, UX ]
          }
        , { name = "Render Conf"
          , link = "http://2016.render-conf.com/"
          , startDate = ( 2016, Apr, 21 )
          , endDate = ( 2016, Apr, 22 )
          , location = "Oxford, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, England, CSS, JavaScript, UX ]
          }
        , { name = "dotSecurity"
          , link = "http://www.dotsecurity.io/"
          , startDate = ( 2016, Apr, 22 )
          , endDate = ( 2016, Apr, 22 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, France, Security ]
          }
        , { name = "Generate NY"
          , link = "http://generateconf.com/new-york-2016"
          , startDate = ( 2016, Apr, 22 )
          , endDate = ( 2016, Apr, 22 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "ProgSCon"
          , link = "http://progscon.co.uk/"
          , startDate = ( 2016, Apr, 22 )
          , endDate = ( 2016, Apr, 22 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jan, 21 )
          , tags = [ English, Developers, England, General ]
          }
        , { name = "Xamarin Evolve"
          , link = "https://evolve.xamarin.com/"
          , startDate = ( 2016, Apr, 24 )
          , endDate = ( 2016, Apr, 28 )
          , location = "Orlando, FL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Mobile, IOS, Android, DotNet ]
          }
        , { name = "dotScale"
          , link = "http://www.dotscale.io/"
          , startDate = ( 2016, Apr, 25 )
          , endDate = ( 2016, Apr, 25 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, France, Scalability ]
          }
        , { name = "OpenVis Conf"
          , link = "https://openvisconf.com/"
          , startDate = ( 2016, Apr, 25 )
          , endDate = ( 2016, Apr, 26 )
          , location = "Boston, MA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, DataVisualization ]
          }
        , { name = "AWS Summit Kuala Lumpur"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Apr, 26 )
          , endDate = ( 2016, Apr, 26 )
          , location = "Kuala Lumpur, Malaysia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Malaysia, AWS, DevOps ]
          }
        , { name = "AWS Summit Sydney"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Apr, 26 )
          , endDate = ( 2016, Apr, 28 )
          , location = "Sydney, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Australia, AWS, DevOps ]
          }
        , { name = "Craft Conf"
          , link = "http://craft-conf.com/2016"
          , startDate = ( 2016, Apr, 26 )
          , endDate = ( 2016, Apr, 29 )
          , location = "Budapest, Hungary"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Hungary, SoftwareCraftsmanship ]
          }
        , { name = "Kafka Summit"
          , link = "http://kafka-summit.org/"
          , startDate = ( 2016, Apr, 26 )
          , endDate = ( 2016, Apr, 26 )
          , location = "San Francisco, CA"
          , cfpStartDate = Just ( 2015, Sep, 29 )
          , cfpEndDate = Just ( 2016, Jan, 11 )
          , tags = [ English, Developers, USA, BigData ]
          }
        , { name = "TestIstanbul"
          , link = "http://testistanbul.org/"
          , startDate = ( 2016, Apr, 26 )
          , endDate = ( 2016, Apr, 26 )
          , location = "Istanbul, Turkey"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Turkish, Developers, Turkey, Testing ]
          }
        , { name = "droidcon Zagreb"
          , link = "http://droidcon.hr/en/"
          , startDate = ( 2016, Apr, 27 )
          , endDate = ( 2016, Apr, 29 )
          , location = "Zagreb, Croatia"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 1 )
          , tags = [ English, Developers, Croatia, Android, Mobile ]
          }
        , { name = "Squares Conference"
          , link = "http://squaresconference.com/"
          , startDate = ( 2016, Apr, 27 )
          , endDate = ( 2016, Apr, 29 )
          , location = "Grapevine, TX"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "University of Illinois WebCon"
          , link = "http://webcon.illinois.edu/"
          , startDate = ( 2016, Apr, 27 )
          , endDate = ( 2016, Apr, 28 )
          , location = "Champaign, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "AWS Summit Buenos Aires"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Apr, 28 )
          , endDate = ( 2016, Apr, 28 )
          , location = "Buenos Aires, Argentina"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Argentina, AWS, DevOps ]
          }
        , { name = "AWS Summit Singapore"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Apr, 28 )
          , endDate = ( 2016, Apr, 28 )
          , location = "Singapore"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Singapore, AWS, DevOps ]
          }
        , { name = "NSNorth"
          , link = "http://nsnorth.ca/"
          , startDate = ( 2016, Apr, 28 )
          , endDate = ( 2016, Apr, 30 )
          , location = "Toronto, Ontario, Canada"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Canada, IOS ]
          }
        , { name = "YOW! Lambda Jam"
          , link = "http://lambdajam.yowconference.com.au/"
          , startDate = ( 2016, Apr, 28 )
          , endDate = ( 2016, Apr, 29 )
          , location = "Brisbane, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 13 )
          , tags = [ English, Developers, Australia, FunctionalProgramming ]
          }
        , { name = "JSDayES"
          , link = "http://jsday.es/"
          , startDate = ( 2016, Apr, 29 )
          , endDate = ( 2016, Apr, 30 )
          , location = "Madrid, Spain"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 28 )
          , tags = [ Spanish, Developers, Spain, JavaScript, NodeJS, AngularJS ]
          }
        , { name = "Chicago Code Camp"
          , link = "http://www.chicagocodecamp.com/"
          , startDate = ( 2016, Apr, 30 )
          , endDate = ( 2016, Apr, 30 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 1 )
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "flatMap(Oslo)"
          , link = "http://2016.flatmap.no/"
          , startDate = ( 2016, May, 2 )
          , endDate = ( 2016, May, 3 )
          , location = "Oslo, Norway"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 3 )
          , tags = [ English, Developers, Norway, Scala, FunctionalProgramming, Java ]
          }
        , { name = "AWS Summit Santiago"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 3 )
          , endDate = ( 2016, May, 3 )
          , location = "Santiago, Chile"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Chile, AWS, DevOps ]
          }
        , { name = "Continuous Lifecycle London"
          , link = "http://continuouslifecycle.london/"
          , startDate = ( 2016, May, 3 )
          , endDate = ( 2016, May, 5 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, DevOps ]
          }
        , { name = "JSConf Belgium"
          , link = "https://jsconf.be/nl/"
          , startDate = ( 2016, May, 3 )
          , endDate = ( 2016, May, 3 )
          , location = "Brugge, Belgium"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Belgium, JavaScript ]
          }
        , { name = "YOW! West"
          , link = "http://west.yowconference.com.au/"
          , startDate = ( 2016, May, 3 )
          , endDate = ( 2016, May, 4 )
          , location = "Perth, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 13 )
          , tags = [ English, Developers, Australia, General ]
          }
        , { name = "ng-conf"
          , link = "http://www.ng-conf.org/"
          , startDate = ( 2016, May, 4 )
          , endDate = ( 2016, May, 6 )
          , location = "Salt Lake City, UT"
          , cfpStartDate = Just ( 2016, Jan, 13 )
          , cfpEndDate = Just ( 2016, Feb, 14 )
          , tags = [ English, Developers, USA, JavaScript, AngularJS ]
          }
        , { name = "AWS Summit Stockholm"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 4 )
          , endDate = ( 2016, May, 4 )
          , location = "Stockholm, Sweden"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Sweden, AWS, DevOps ]
          }
        , { name = "RailsConf"
          , link = "http://railsconf.com/"
          , startDate = ( 2016, May, 4 )
          , endDate = ( 2016, May, 6 )
          , location = "Kansas City, MO"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jan, 15 )
          , tags = [ English, Developers, USA, Ruby, Rails ]
          }
        , { name = "Typelevel Summit"
          , link = "http://typelevel.org/event/2016-05-summit-oslo/"
          , startDate = ( 2016, May, 4 )
          , endDate = ( 2016, May, 4 )
          , location = "Oslo, Norway"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 3 )
          , tags = [ English, Developers, Norway, Scala, FunctionalProgramming, Java ]
          }
        , { name = "AWS Summit Manila"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 5 )
          , endDate = ( 2016, May, 5 )
          , location = "Manila, Philippines"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Philippines, AWS, DevOps ]
          }
        , { name = "CocoaConf Seattle"
          , link = "http://cocoaconf.com/seattle-2016/home"
          , startDate = ( 2016, May, 6 )
          , endDate = ( 2016, May, 7 )
          , location = "Seattle, WA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, IOS, Cocoa ]
          }
        , { name = "SyntaxCon"
          , link = "http://2016.syntaxcon.com/"
          , startDate = ( 2016, May, 6 )
          , endDate = ( 2016, May, 7 )
          , location = "Charleston, SC"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 12 )
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "!!Con"
          , link = "http://bangbangcon.com/"
          , startDate = ( 2016, May, 7 )
          , endDate = ( 2016, May, 8 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 18 )
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "9th European Lisp Symposium"
          , link = "http://www.european-lisp-symposium.org/"
          , startDate = ( 2016, May, 9 )
          , endDate = ( 2016, May, 10 )
          , location = "Krakow, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 19 )
          , tags = [ English, Developers, Poland, Lisp, Clojure ]
          }
        , { name = "Apache: Big Data North America"
          , link = "http://www.apachecon.com/"
          , startDate = ( 2016, May, 9 )
          , endDate = ( 2016, May, 11 )
          , location = "Vancouver, BC, Canada"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 12 )
          , tags = [ English, Developers, Canada, BigData ]
          }
        , { name = "Beyond Tellerrand"
          , link = "http://beyondtellerrand.com/events/duesseldorf-2016"
          , startDate = ( 2016, May, 9 )
          , endDate = ( 2016, May, 11 )
          , location = "Düsseldorf, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, Germany, UX, General ]
          }
        , { name = "C++Now"
          , link = "http://cppnow.org/"
          , startDate = ( 2016, May, 9 )
          , endDate = ( 2016, May, 14 )
          , location = "Aspen, CO"
          , cfpStartDate = Just ( 2015, Nov, 17 )
          , cfpEndDate = Just ( 2016, Jan, 29 )
          , tags = [ English, Developers, USA, CPlusPlus ]
          }
        , { name = "ChefConf"
          , link = "https://www.chef.io/chefconf/"
          , startDate = ( 2016, May, 9 )
          , endDate = ( 2016, May, 11 )
          , location = "Austin, TX"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 29 )
          , tags = [ English, Developers, USA, Chef, DevOps ]
          }
        , { name = "DrupalCon New Orleans"
          , link = "https://events.drupal.org/neworleans2016/"
          , startDate = ( 2016, May, 9 )
          , endDate = ( 2016, May, 13 )
          , location = "New Orleans, LA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 29 )
          , tags = [ English, Developers, USA, Drupal, PHP ]
          }
        , { name = "Scala Days New York"
          , link = "http://event.scaladays.org/scaladays-nyc-2016"
          , startDate = ( 2016, May, 9 )
          , endDate = ( 2016, May, 11 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Scala ]
          }
        , { name = "Codemotion Amsterdam"
          , link = "http://amsterdam2016.codemotionworld.com/"
          , startDate = ( 2016, May, 11 )
          , endDate = ( 2016, May, 12 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Netherlands, General ]
          }
        , { name = "CSSConf Budapest"
          , link = "http://cssconfbp.rocks/"
          , startDate = ( 2016, May, 11 )
          , endDate = ( 2016, May, 11 )
          , location = "Budapest, Hungary"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Hungary, CSS ]
          }
        , { name = "ElixirConf EU"
          , link = "http://www.elixirconf.eu/"
          , startDate = ( 2016, May, 11 )
          , endDate = ( 2016, May, 12 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 18 )
          , tags = [ English, Developers, Germany, Elixir, FunctionalProgramming, Erlang ]
          }
        , { name = "jsDay"
          , link = "http://2016.jsday.it/"
          , startDate = ( 2016, May, 11 )
          , endDate = ( 2016, May, 12 )
          , location = "Verona, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jan, 31 )
          , tags = [ English, Developers, Italy, JavaScript ]
          }
        , { name = "React Remote Conf"
          , link = "https://allremoteconfs.com/react-2016"
          , startDate = ( 2016, May, 11 )
          , endDate = ( 2016, May, 13 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 9 )
          , tags = [ English, Developers, Remote, JavaScript, React ]
          }
        , { name = "UX Alive"
          , link = "http://www.uxalive.com/"
          , startDate = ( 2016, May, 11 )
          , endDate = ( 2016, May, 13 )
          , location = "Istanbul, Turkey"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Turkey, UX ]
          }
        , { name = "ApacheCon Core North America"
          , link = "http://www.apachecon.com/"
          , startDate = ( 2016, May, 12 )
          , endDate = ( 2016, May, 13 )
          , location = "Vancouver, Canada"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 12 )
          , tags = [ English, Developers, Canada, OpenSource ]
          }
        , { name = "Front Conference"
          , link = "http://www.frontutah.com/"
          , startDate = ( 2016, May, 12 )
          , endDate = ( 2016, May, 13 )
          , location = "Salt Lake City, UT"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "JSConf Budapest"
          , link = "http://jsconfbp.com/"
          , startDate = ( 2016, May, 12 )
          , endDate = ( 2016, May, 13 )
          , location = "Budapest, Hungary"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Hungary, JavaScript ]
          }
        , { name = "An Event Apart Boston"
          , link = "http://aneventapart.com/event/boston-2016"
          , startDate = ( 2016, May, 16 )
          , endDate = ( 2016, May, 18 )
          , location = "Boston, MA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, UX ]
          }
        , { name = "Open Source Convention Tutorials"
          , link = "http://conferences.oreilly.com/oscon/open-source"
          , startDate = ( 2016, May, 16 )
          , endDate = ( 2016, May, 17 )
          , location = "Austin, TX"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, OpenSource ]
          }
        , { name = "AWS Summit Seoul"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 17 )
          , endDate = ( 2016, May, 17 )
          , location = "Seoul, South Korea"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, SouthKorea, AWS, DevOps ]
          }
        , { name = "Front-Trends"
          , link = "http://2016.front-trends.com/"
          , startDate = ( 2016, May, 18 )
          , endDate = ( 2016, May, 20 )
          , location = "Warsaw, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 4 )
          , tags = [ English, Designers, Poland, UX ]
          }
        , { name = "Open Source Convention"
          , link = "http://conferences.oreilly.com/oscon/open-source"
          , startDate = ( 2016, May, 18 )
          , endDate = ( 2016, May, 19 )
          , location = "Austin, TX"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, OpenSource ]
          }
        , { name = "UX London"
          , link = "http://2016.uxlondon.com/"
          , startDate = ( 2016, May, 18 )
          , endDate = ( 2016, May, 20 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, England, UX ]
          }
        , { name = "droidcon Montreal"
          , link = "http://www.droidcon.ca/"
          , startDate = ( 2016, May, 19 )
          , endDate = ( 2016, May, 20 )
          , location = "Montreal, Canada"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 4 )
          , tags = [ English, Developers, Canada, Android, Mobile ]
          }
        , { name = "PhoneGap Day EU"
          , link = "http://pgday.phonegap.com/eu2016/"
          , startDate = ( 2016, May, 19 )
          , endDate = ( 2016, May, 20 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Netherlands, PhoneGap, Mobile ]
          }
        , { name = "Valio Con"
          , link = "http://valiocon.com/"
          , startDate = ( 2016, May, 19 )
          , endDate = ( 2016, May, 22 )
          , location = "San Diego, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "AWS Summit Taipei"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 20 )
          , endDate = ( 2016, May, 20 )
          , location = "Taipei, Taiwan"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Taiwan, AWS, DevOps ]
          }
        , { name = "self.conference"
          , link = "http://selfconference.org/"
          , startDate = ( 2016, May, 20 )
          , endDate = ( 2016, May, 21 )
          , location = "Detroit, MI"
          , cfpStartDate = Just ( 2016, Jan, 18 )
          , cfpEndDate = Just ( 2016, Feb, 15 )
          , tags = [ English, Designers, Developers, USA, General, SoftSkills ]
          }
        , { name = "EmpEx"
          , link = "http://empex.co/"
          , startDate = ( 2016, May, 21 )
          , endDate = ( 2016, May, 21 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2015, Mar, 15 )
          , tags = [ English, Developers, USA, Elixir, FunctionalProgramming ]
          }
        , { name = "UIKonf"
          , link = "http://www.uikonf.com/"
          , startDate = ( 2016, May, 22 )
          , endDate = ( 2016, May, 25 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Germany, IOS ]
          }
        , { name = "GOTO Chicago Workshops"
          , link = "http://gotocon.com/chicago-2016"
          , startDate = ( 2016, May, 23 )
          , endDate = ( 2016, May, 23 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "php[tek]"
          , link = "https://tek.phparch.com/"
          , startDate = ( 2016, May, 23 )
          , endDate = ( 2016, May, 27 )
          , location = "St. Louis, MO"
          , cfpStartDate = Just ( 2015, Dec, 14 )
          , cfpEndDate = Just ( 2016, Jan, 16 )
          , tags = [ English, Developers, USA, PHP ]
          }
        , { name = "AWS Summit Netherlands"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 24 )
          , endDate = ( 2016, May, 24 )
          , location = "Nieuwegein, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Netherlands, AWS, DevOps ]
          }
        , { name = "GOTO Chicago"
          , link = "http://gotocon.com/chicago-2016"
          , startDate = ( 2016, May, 24 )
          , endDate = ( 2016, May, 25 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "AWS Summit Mumbai"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 25 )
          , endDate = ( 2016, May, 25 )
          , location = "Mumbai, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, India, AWS, DevOps ]
          }
        , { name = "HBaseCon"
          , link = "http://www.hbasecon.com/"
          , startDate = ( 2016, May, 25 )
          , endDate = ( 2016, May, 25 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 28 )
          , tags = [ English, Developers, USA, Hadoop, BigData, Cloud ]
          }
        , { name = "SIGNAL 2016"
          , link = "https://www.twilio.com/signal"
          , startDate = ( 2016, May, 24 )
          , endDate = ( 2016, May, 25 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Communications ]
          }
        , { name = "UXLx"
          , link = "https://www.ux-lx.com/"
          , startDate = ( 2016, May, 24 )
          , endDate = ( 2016, May, 27 )
          , location = "Lisbon, Portugal"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Portugal, UX ]
          }
        , { name = "GlueCon"
          , link = "http://gluecon.com/"
          , startDate = ( 2016, May, 25 )
          , endDate = ( 2016, May, 26 )
          , location = "Broomfield, CO"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, May, 1 )
          , tags = [ English, Developers, USA, General, DevOps, BigData ]
          }
        , { name = "PrlConf"
          , link = "http://www.jonprl.org/prlconf"
          , startDate = ( 2016, May, 25 )
          , endDate = ( 2016, May, 25 )
          , location = "Boulder, CO"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, FunctionalProgramming ]
          }
        , { name = "PureScript Conf"
          , link = "https://github.com/purescript/purescript/wiki/PureScript-Conf-2016"
          , startDate = ( 2016, May, 25 )
          , endDate = ( 2016, May, 25 )
          , location = "Boulder, CO"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, FunctionalProgramming, PureScript ]
          }
        , { name = "AWS Summit Mexico City"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 26 )
          , endDate = ( 2016, May, 26 )
          , location = "Mexico City, Mexico"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Mexico, AWS, DevOps ]
          }
        , { name = "DevSum"
          , link = "http://www.devsum.se/"
          , startDate = ( 2016, May, 26 )
          , endDate = ( 2016, May, 27 )
          , location = "Stockholm, Sweden"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 13 )
          , tags = [ English, Developers, Sweden, DotNet ]
          }
        , { name = "GOTO Chicago Workshops"
          , link = "http://gotocon.com/chicago-2016"
          , startDate = ( 2016, May, 26 )
          , endDate = ( 2016, May, 26 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "iOSCon"
          , link = "https://skillsmatter.com/conferences/7598-ioscon-2016-the-conference-for-ios-and-swift-developers"
          , startDate = ( 2016, May, 26 )
          , endDate = ( 2016, May, 27 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, IOS ]
          }
        , { name = "LambdaConf"
          , link = "http://lambdaconf.us/"
          , startDate = ( 2016, May, 26 )
          , endDate = ( 2016, May, 29 )
          , location = "Boulder, CO"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, FunctionalProgramming, Haskell, Scala, PureScript ]
          }
        , { name = "Frontend United"
          , link = "http://frontendunited.org/"
          , startDate = ( 2016, May, 27 )
          , endDate = ( 2016, May, 28 )
          , location = "Ghent, Belgium"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, Belgium, UX, Drupal, PHP ]
          }
        , { name = "PyCon Tutorials"
          , link = "https://us.pycon.org/2016/"
          , startDate = ( 2016, May, 28 )
          , endDate = ( 2016, May, 29 )
          , location = "Portland, OR"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Python ]
          }
        , { name = "PyCon"
          , link = "https://us.pycon.org/2016/"
          , startDate = ( 2016, May, 30 )
          , endDate = ( 2016, Jun, 1 )
          , location = "Portland, OR"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Python ]
          }
        , { name = "AWS Summit Paris"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, May, 31 )
          , endDate = ( 2016, May, 31 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, France, AWS, DevOps ]
          }
        , { name = "AWS Summit Tokyo"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Jun, 1 )
          , endDate = ( 2016, Jun, 3 )
          , location = "Tokyo, Japan"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Japan, AWS, DevOps ]
          }
        , { name = "CSSConf Nordic"
          , link = "http://cssconf.no/"
          , startDate = ( 2016, Jun, 1 )
          , endDate = ( 2016, Jun, 1 )
          , location = "Oslo, Norway"
          , cfpStartDate = Just ( 2015, Dec, 23 )
          , cfpEndDate = Just ( 2016, Jan, 31 )
          , tags = [ English, Designers, Norway, UX, CSS ]
          }
        , { name = "Git Remote Conf"
          , link = "https://allremoteconfs.com/git-2016"
          , startDate = ( 2016, Jun, 1 )
          , endDate = ( 2016, Jun, 3 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, May, 6 )
          , tags = [ English, Designers, Developers, Remote, Git ]
          }
        , { name = "MagmaConf"
          , link = "http://www.magmaconf.com/"
          , startDate = ( 2016, Jun, 1 )
          , endDate = ( 2016, Jun, 3 )
          , location = "Manzanillo, Mexico"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Mexico, Ruby ]
          }
        , { name = "ScotlandCSS"
          , link = "http://scotlandcss.launchrock.com/"
          , startDate = ( 2016, Jun, 1 )
          , endDate = ( 2016, Jun, 1 )
          , location = "Edinburgh, Scotland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Scotland, CSS ]
          }
        , { name = "AWS Summit Madrid"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Jun, 2 )
          , endDate = ( 2016, Jun, 2 )
          , location = "Madrid, Spain"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Spain, AWS, DevOps ]
          }
        , { name = "AWS Summit Sao Paulo"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Jun, 2 )
          , endDate = ( 2016, Jun, 2 )
          , location = "Sao Paulo, Brazil"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Brazil, AWS, DevOps ]
          }
        , { name = "GR8Conf EU"
          , link = "http://gr8conf.eu/"
          , startDate = ( 2016, Jun, 2 )
          , endDate = ( 2016, Jun, 4 )
          , location = "Copenhagen, Denmark"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 13 )
          , tags = [ English, Developers, Denmark, Java, Groovy, Grails, Gradle ]
          }
        , { name = "PyCon Sprints"
          , link = "https://us.pycon.org/2016/"
          , startDate = ( 2016, Jun, 2 )
          , endDate = ( 2016, Jun, 5 )
          , location = "Portland, OR"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Python ]
          }
        , { name = "ReactEurope"
          , link = "https://www.react-europe.org/"
          , startDate = ( 2016, Jun, 2 )
          , endDate = ( 2016, Jun, 3 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, France, React, JavaScript ]
          }
        , { name = "Scotland JS"
          , link = "http://scotlandjs.com/"
          , startDate = ( 2016, Jun, 2 )
          , endDate = ( 2016, Jun, 3 )
          , location = "Edinburgh, Scotland"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 12 )
          , tags = [ English, Developers, Scotland, JavaScript ]
          }
        , { name = "SoCraTes England"
          , link = "http://socratesuk.org/"
          , startDate = ( 2016, Jun, 2 )
          , endDate = ( 2016, Jun, 5 )
          , location = "Dorking, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, SoftwareCraftsmanship ]
          }
        , { name = "Web Rebels"
          , link = "https://www.webrebels.org/"
          , startDate = ( 2016, Jun, 2 )
          , endDate = ( 2016, Jun, 3 )
          , location = "Oslo, Norway"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Norway, JavaScript ]
          }
        , { name = "NodeConf Oslo"
          , link = "http://oslo.nodeconf.com/"
          , startDate = ( 2016, Jun, 4 )
          , endDate = ( 2016, Jun, 4 )
          , location = "Oslo, Norway"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 13 )
          , tags = [ English, Developers, Norway, NodeJS, JavaScript ]
          }
        , { name = "Berlin Buzzwords"
          , link = "http://berlinbuzzwords.de/"
          , startDate = ( 2016, Jun, 5 )
          , endDate = ( 2016, Jun, 7 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 7 )
          , tags = [ English, Developers, Germany, BigData ]
          }
        , { name = "NDC Oslo Workshops"
          , link = "http://ndcoslo.com/"
          , startDate = ( 2016, Jun, 6 )
          , endDate = ( 2016, Jun, 7 )
          , location = "Oslo, Norway"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 15 )
          , tags = [ English, Developers, Norway, Agile, DotNet, General ]
          }
        , { name = "Spark Summit"
          , link = "https://spark-summit.org/2016/"
          , startDate = ( 2016, Jun, 6 )
          , endDate = ( 2016, Jun, 8 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 29 )
          , tags = [ English, Developers, USA, BigData ]
          }
        , { name = "NDC Oslo"
          , link = "http://ndcoslo.com/"
          , startDate = ( 2016, Jun, 8 )
          , endDate = ( 2016, Jun, 10 )
          , location = "Oslo, Norway"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Norway, Agile, DotNet, General ]
          }
        , { name = "UX Scotland"
          , link = "http://uxscotland.net/2016/"
          , startDate = ( 2016, Jun, 8 )
          , endDate = ( 2016, Jun, 10 )
          , location = "Edinburgh, Scotland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Scotland, UX ]
          }
        , { name = "NodeConf"
          , link = "http://nodeconf.com/"
          , startDate = ( 2016, Jun, 9 )
          , endDate = ( 2016, Jun, 12 )
          , location = "Petaluma, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, NodeJS, JavaScript ]
          }
        , { name = "GOTO Amsterdam Workshops"
          , link = "http://gotocon.com/amsterdam-2016/"
          , startDate = ( 2016, Jun, 13 )
          , endDate = ( 2016, Jun, 13 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Netherlands, General ]
          }
        , { name = "QCon New York"
          , link = "https://qconnewyork.com/"
          , startDate = ( 2016, Jun, 13 )
          , endDate = ( 2016, Jun, 15 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "enterJS"
          , link = "https://www.enterjs.de/"
          , startDate = ( 2016, Jun, 14 )
          , endDate = ( 2016, Jun, 16 )
          , location = "Darmstadt, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ German, Developers, Germany, JavaScript ]
          }
        , { name = "GOTO Amsterdam"
          , link = "http://gotocon.com/amsterdam-2016/"
          , startDate = ( 2016, Jun, 14 )
          , endDate = ( 2016, Jun, 15 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Just ( 2016, Jan, 14 )
          , cfpEndDate = Just ( 2016, Mar, 13 )
          , tags = [ English, Developers, Netherlands, General ]
          }
        , { name = "droidcon Berlin"
          , link = "http://droidcon.de/"
          , startDate = ( 2016, Jun, 15 )
          , endDate = ( 2016, Jun, 17 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 28 )
          , tags = [ English, Developers, Germany, Android, Mobile ]
          }
        , { name = "Front End Design Conference"
          , link = "http://frontenddesignconference.com/"
          , startDate = ( 2016, Jun, 15 )
          , endDate = ( 2016, Jun, 17 )
          , location = "St. Petersburg, FL"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 15 )
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "Scala Days Berlin"
          , link = "http://event.scaladays.org/scaladays-berlin-2016"
          , startDate = ( 2016, May, 15 )
          , endDate = ( 2016, May, 17 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Germany, Scala ]
          }
        , { name = "AWS Summit Tel Aviv"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Jun, 16 )
          , endDate = ( 2016, Jun, 16 )
          , location = "Tel Aviv, Israel"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Israel, AWS, DevOps ]
          }
        , { name = "CSS Day"
          , link = "http://cssday.nl/2016"
          , startDate = ( 2016, Jun, 16 )
          , endDate = ( 2016, Jun, 17 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Netherlands, CSS ]
          }
        , { name = "QCon New York Tutorials"
          , link = "https://qconnewyork.com/"
          , startDate = ( 2016, Jun, 16 )
          , endDate = ( 2016, Jun, 17 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "Joy of Coding"
          , link = "http://joyofcoding.org/"
          , startDate = ( 2016, Jun, 17 )
          , endDate = ( 2016, Jun, 17 )
          , location = "Rotterdam, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 17 )
          , tags = [ English, Developers, Netherlands, General ]
          }
        , { name = "Nordic Ruby"
          , link = "http://www.nordicruby.org/"
          , startDate = ( 2016, Jun, 17 )
          , endDate = ( 2016, Jun, 19 )
          , location = "Stockholm, Sweden"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Sweden, Ruby ]
          }
        , { name = "DockerCon"
          , link = "http://2016.dockercon.com/"
          , startDate = ( 2016, Jun, 19 )
          , endDate = ( 2016, Jun, 21 )
          , location = "Seattle, WA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 18 )
          , tags = [ English, Developers, USA, Docker ]
          }
        , { name = "Public Sector Summit"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Jun, 20 )
          , endDate = ( 2016, Jun, 21 )
          , location = "Washington, DC"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, AWS, DevOps ]
          }
        , { name = "O'Reilly Velocity"
          , link = "http://conferences.oreilly.com/velocity/devops-web-performance-ca/"
          , startDate = ( 2016, Jun, 21 )
          , endDate = ( 2016, Jun, 23 )
          , location = "Santa Clara, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jan, 11 )
          , tags = [ English, Developers, USA, Scalability, DevOps ]
          }
        , { name = "Codemotion Dublin"
          , link = "http://dublin2016.codemotionworld.com/"
          , startDate = ( 2016, Jun, 22 )
          , endDate = ( 2016, Jun, 23 )
          , location = "Dublin, Ireland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Ireland, General ]
          }
        , { name = "RedDotRubyConf"
          , link = "http://www.reddotrubyconf.com/"
          , startDate = ( 2016, Jun, 23 )
          , endDate = ( 2016, Jun, 24 )
          , location = "Singapore"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 28 )
          , tags = [ English, Developers, Singapore, Ruby ]
          }
        , { name = "Web Design Day"
          , link = "http://webdesignday.com/"
          , startDate = ( 2016, Jun, 23 )
          , endDate = ( 2016, Jun, 24 )
          , location = "Pittsburgh, PA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "Dinosaur.js"
          , link = "http://dinosaurjs.org/"
          , startDate = ( 2016, Jun, 24 )
          , endDate = ( 2016, Jun, 24 )
          , location = "Denver, CO"
          , cfpStartDate = Just ( 2016, Feb, 1 )
          , cfpEndDate = Just ( 2016, Mar, 14 )
          , tags = [ English, Developers, USA, JavaScript ]
          }
        , { name = "GIANT Conf"
          , link = "http://www.giantux.com/conf2016/"
          , startDate = ( 2016, Jun, 27 )
          , endDate = ( 2016, Jun, 29 )
          , location = "TBD"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "You Gotta Love Frontend"
          , link = "http://yougottalovefrontend.com/"
          , startDate = ( 2016, Jun, 27 )
          , endDate = ( 2016, Jun, 28 )
          , location = "Tel Aviv, Israel"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 31 )
          , tags = [ English, Designers, Developers, Israel, JavaScript, AngularJS, React, CSS, General ]
          }
        , { name = "Hadoop Summit North America"
          , link = "http://2016.hadoopsummit.org/san-jose/"
          , startDate = ( 2016, Jun, 28 )
          , endDate = ( 2016, Jun, 30 )
          , location = "San Jose, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Hadoop, BigData, InternetOfThings ]
          }
        , { name = "MongoDB World 2016"
          , link = "https://www.mongodb.com/world16"
          , startDate = ( 2016, Jun, 28 )
          , endDate = ( 2016, Jun, 29 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, MongoDB, BigData ]
          }
        , { name = "AWS Summit Auckland"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Jun, 29 )
          , endDate = ( 2016, Jun, 29 )
          , location = "Auckland, New Zealand"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, NewZealand, AWS, DevOps ]
          }
        , { name = "PolyConf"
          , link = "http://polyconf.com/"
          , startDate = ( 2016, Jun, 30 )
          , endDate = ( 2016, Jul, 2 )
          , location = "Poznan, Poland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Poland, General ]
          }
        , { name = "AWS Summit London"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Jul, 6 )
          , endDate = ( 2016, Jul, 7 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, AWS, DevOps ]
          }
        , { name = "Brighton Ruby"
          , link = "http://brightonruby.com/"
          , startDate = ( 2016, Jul, 8 )
          , endDate = ( 2016, Jul, 8 )
          , location = "Brighton, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 31 )
          , tags = [ English, Developers, England, Ruby ]
          }
        , { name = "Chef Conf"
          , link = "https://www.chef.io/chefconf/"
          , startDate = ( 2016, Jul, 11 )
          , endDate = ( 2016, Jul, 13 )
          , location = "Austin, TX"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 29 )
          , tags = [ English, Developers, USA, Chef, DevOps ]
          }
        , { name = "GopherCon"
          , link = "https://www.gophercon.com/"
          , startDate = ( 2016, Jul, 11 )
          , endDate = ( 2016, Jul, 13 )
          , location = "Denver, CO"
          , cfpStartDate = Just ( 2016, Jan, 1 )
          , cfpEndDate = Just ( 2016, Jan, 31 )
          , tags = [ English, Developers, USA, Go ]
          }
        , { name = "AWS Summit Santa Clara"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Jul, 12 )
          , endDate = ( 2016, Jul, 13 )
          , location = "Santa Clara, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, AWS, DevOps ]
          }
        , { name = "Newbie Remote Conf"
          , link = "https://allremoteconfs.com/newbie-2016"
          , startDate = ( 2016, Jul, 13 )
          , endDate = ( 2016, Jul, 15 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jun, 11 )
          , tags = [ English, Designers, Developers, Remote, General ]
          }
        , { name = "Generate San Francisco"
          , link = "http://www.generateconf.com/san-francisco-2016/"
          , startDate = ( 2016, Jul, 15 )
          , endDate = ( 2016, Jul, 15 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "EuroPython"
          , link = "http://ep2016.europython.eu/"
          , startDate = ( 2016, Jul, 17 )
          , endDate = ( 2016, Jul, 24 )
          , location = "Bilbao, Spain"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Spain, Python ]
          }
        , { name = "php[cruise]"
          , link = "https://cruise.phparch.com/"
          , startDate = ( 2016, Jul, 17 )
          , endDate = ( 2016, Jul, 24 )
          , location = "Baltimore, MD"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, PHP ]
          }
        , { name = "Curry On"
          , link = "http://curry-on.org/2016/"
          , startDate = ( 2016, Jul, 18 )
          , endDate = ( 2016, Jul, 19 )
          , location = "Rome, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Italy, General ]
          }
        , { name = "UberConf"
          , link = "https://uberconf.com/conference/denver/2016/07/home"
          , startDate = ( 2016, Jul, 19 )
          , endDate = ( 2016, Jul, 22 )
          , location = "Denver, CO"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Java, Agile, Cloud, Scala, Groovy ]
          }
        , { name = "European Conference on Object-Oriented Programming"
          , link = "http://2016.ecoop.org/"
          , startDate = ( 2016, Jul, 20 )
          , endDate = ( 2016, Jul, 22 )
          , location = "Rome, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Italy, General ]
          }
        , { name = "An Event Apart DC"
          , link = "http://aneventapart.com/event/washington-dc-2016"
          , startDate = ( 2016, Jul, 25 )
          , endDate = ( 2016, Jul, 27 )
          , location = "Washington, DC"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, UX ]
          }
        , { name = "Mobile & Web CodeCamp"
          , link = "http://www.mobilewebcodecamp.com/"
          , startDate = ( 2016, Jul, 26 )
          , endDate = ( 2016, Jul, 29 )
          , location = "Salt Lake City, UT"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Mobile, Android, IOS, JavaScript, PhoneGap ]
          }
        , { name = "GR8Conf US"
          , link = "http://gr8conf.org/"
          , startDate = ( 2016, Jul, 27 )
          , endDate = ( 2016, Jul, 29 )
          , location = "Minneapolis, MN"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 15 )
          , tags = [ English, Developers, USA, Java, Groovy, Grails, Gradle ]
          }
        , { name = "Forward 5 Web Summit"
          , link = "http://forwardjs.com/"
          , startDate = ( 2016, Jul, 28 )
          , endDate = ( 2016, Jul, 28 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 15 )
          , tags = [ English, Developers, USA, JavaScript ]
          }
        , { name = "Forward Swift"
          , link = "http://forwardjs.com/"
          , startDate = ( 2016, Jul, 28 )
          , endDate = ( 2016, Jul, 28 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 15 )
          , tags = [ English, Developers, USA, Swift ]
          }
        , { name = "NDC Sydney Workshops"
          , link = "http://ndcsydney.com/"
          , startDate = ( 2016, Aug, 1 )
          , endDate = ( 2016, Aug, 2 )
          , location = "Sydney, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 1 )
          , tags = [ English, Developers, Australia, Agile, DotNet, General ]
          }
        , { name = "AWS Summit Canberra"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Aug, 3 )
          , endDate = ( 2016, Aug, 3 )
          , location = "Canberra, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Australia, AWS, DevOps ]
          }
        , { name = "NDC Sydney"
          , link = "http://ndcsydney.com/"
          , startDate = ( 2016, Aug, 3 )
          , endDate = ( 2016, Aug, 5 )
          , location = "Sydney, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 1 )
          , tags = [ English, Developers, Australia, Agile, DotNet, General ]
          }
        , { name = "CSSconf Argentina"
          , link = "http://cssconfar.com/"
          , startDate = ( 2016, Aug, 7 )
          , endDate = ( 2016, Aug, 7 )
          , location = "Buenos Aires, Argentina"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Argentina, CSS ]
          }
        , { name = "That Conference"
          , link = "https://www.thatconference.com/"
          , startDate = ( 2016, Aug, 8 )
          , endDate = ( 2016, Aug, 10 )
          , location = "Wisconsin Dells, WI"
          , cfpStartDate = Just ( 2016, Mar, 1 )
          , cfpEndDate = Just ( 2016, Mar, 31 )
          , tags = [ English, Developers, USA, Mobile, Cloud ]
          }
        , { name = "AWS Summit New York"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Aug, 10 )
          , endDate = ( 2016, Aug, 11 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, AWS, DevOps ]
          }
        , { name = "Midwest JS"
          , link = "http://midwestjs.com/"
          , startDate = ( 2016, Aug, 10 )
          , endDate = ( 2016, Aug, 12 )
          , location = "Minneapolis, MN"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 5 )
          , tags = [ English, Developers, USA, JavaScript, NodeJS ]
          }
        , { name = "Robots Remote Conf"
          , link = "https://allremoteconfs.com/robots-2016"
          , startDate = ( 2016, Aug, 10 )
          , endDate = ( 2016, Aug, 12 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jul, 9 )
          , tags = [ English, Developers, Remote, Robotics ]
          }
        , { name = "FP Conf"
          , link = "http://fpconf.org/"
          , startDate = ( 2016, Aug, 15 )
          , endDate = ( 2016, Aug, 15 )
          , location = "Moscow, Russia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Russian, Developers, Russia, FunctionalProgramming, Erlang, Scala, Clojure, Haskell ]
          }
        , { name = "Abstractions"
          , link = "http://abstractions.io/"
          , startDate = ( 2016, Aug, 18 )
          , endDate = ( 2016, Aug, 20 )
          , location = "Pittsburgh, PA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 31 )
          , tags = [ English, Developers, USA, General ]
          }
        , { name = "HybridConf"
          , link = "https://hybridconf.net/"
          , startDate = ( 2016, Aug, 18 )
          , endDate = ( 2016, Aug, 19 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Germany, General, UX ]
          }
        , { name = "360|iDev"
          , link = "http://360idev.com/"
          , startDate = ( 2016, Aug, 21 )
          , endDate = ( 2016, Aug, 24 )
          , location = "Denver, CO"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, IOS ]
          }
        , { name = "JSConf Iceland"
          , link = "http://2016.jsconf.is/"
          , startDate = ( 2016, Aug, 25 )
          , endDate = ( 2016, Aug, 26 )
          , location = "Reykjavik, Iceland"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 31 )
          , tags = [ English, Developers, Iceland, JavaScript ]
          }
        , { name = "React Rally"
          , link = "http://www.reactrally.com/"
          , startDate = ( 2016, Aug, 25 )
          , endDate = ( 2016, Aug, 26 )
          , location = "Salt Lake City, UT"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, React, JavaScript ]
          }
        , { name = "BrazilJS"
          , link = "https://braziljs.org/conf"
          , startDate = ( 2016, Aug, 26 )
          , endDate = ( 2016, Aug, 27 )
          , location = "Porto Alegre, Brazil"
          , cfpStartDate = Nothing
          , cfpEndDate = Just (2016, Apr, 30)
          , tags = [ English, Spanish, Portuguese, Developers, Brazil, JavaScript ]
          }
        , { name = "AlterConf South Africa"
          , link = "http://www.alterconf.com/sessions/cape-town-south-africa"
          , startDate = ( 2016, Aug, 27 )
          , endDate = ( 2016, Aug, 27 )
          , location = "Cape Town, South Africa"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jun, 16 )
          , tags = [ English, Designers, Developers, SouthAfrica, Diversity, SoftSkills ]
          }
        , { name = "An Event Apart Chicago"
          , link = "http://aneventapart.com/event/chicago-2016"
          , startDate = ( 2016, Aug, 29 )
          , endDate = ( 2016, Aug, 31 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, UX ]
          }
        , { name = "Agile on the Beach"
          , link = "http://agileonthebeach.com/2016-2/"
          , startDate = ( 2016, Sep, 1 )
          , endDate = ( 2016, Sep, 2 )
          , location = "Falmouth, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 29 )
          , tags = [ English, Developers, England, Agile ]
          }
        , { name = "ColdFront"
          , link = "https://2016.coldfrontconf.com/"
          , startDate = ( 2016, Sep, 1 )
          , endDate = ( 2016, Sep, 1 )
          , location = "Copenhagen, Denmark"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Designers, Denmark, JavaScript, CSS, Mobile ]
          }
        , { name = "Frontend Conference Zurich"
          , link = "https://frontendconf.ch/"
          , startDate = ( 2016, Sep, 1 )
          , endDate = ( 2016, Sep, 2 )
          , location = "Zürich, Switzerland"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, May, 31 )
          , tags = [ English, Designers, Switzerland, UX ]
          }
        , { name = "Full Stack Fest"
          , link = "http://2016.fullstackfest.com/"
          , startDate = ( 2016, Sep, 5 )
          , endDate = ( 2016, Sep, 9 )
          , location = "Barcelona, Spain"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Spain, Ruby, JavaScript, General ]
          }
        , { name = "Generate Sydney"
          , link = "http://www.generateconf.com/"
          , startDate = ( 2016, Sep, 5 )
          , endDate = ( 2016, Sep, 5 )
          , location = "Sydney, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Australia, UX ]
          }
        , { name = "iOSDevEngland"
          , link = "http://www.iosdevuk.com/"
          , startDate = ( 2016, Sep, 5 )
          , endDate = ( 2016, Sep, 8 )
          , location = "Aberystwyth, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, IOS, Swift ]
          }
        , { name = "RubyKaigi"
          , link = "http://rubykaigi.org/2016"
          , startDate = ( 2016, Sep, 8 )
          , endDate = ( 2016, Sep, 10 )
          , location = "Kyoto, Japan"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, English, Japanese, Ruby ]
          }
        , { name = "CocoaConf DC"
          , link = "http://cocoaconf.com/dc-2016/home"
          , startDate = ( 2016, Sep, 9 )
          , endDate = ( 2016, Sep, 10 )
          , location = "Washington, DC"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Cocoa, IOS ]
          }
        , { name = "Agile Prague"
          , link = "http://agileprague.com/"
          , startDate = ( 2016, Sep, 12 )
          , endDate = ( 2016, Sep, 13 )
          , location = "Prague, Czech Republic"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jun, 30 )
          , tags = [ English, Developers, CzechRepublic, Agile ]
          }
        , { name = "SwanseaCon"
          , link = "http://swanseacon.co.uk/"
          , startDate = ( 2016, Sep, 12 )
          , endDate = ( 2016, Sep, 13 )
          , location = "Swansea, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Feb, 15 )
          , tags = [ English, Developers, England, Agile, SoftwareCraftsmanship ]
          }
        , { name = "Angular Remote Conf"
          , link = "https://allremoteconfs.com/angular-2016"
          , startDate = ( 2016, Sep, 14 )
          , endDate = ( 2016, Sep, 16 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Aug, 13 )
          , tags = [ English, Developers, Remote, JavaScript, AngularJS ]
          }
        , { name = "From the Front"
          , link = "http://2016.fromthefront.it/"
          , startDate = ( 2016, Sep, 15 )
          , endDate = ( 2016, Sep, 16 )
          , location = "Bologna, Italy"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 15 )
          , tags = [ English, Designers, Italy, UX ]
          }
        , { name = "Strangeloop"
          , link = "http://thestrangeloop.com/"
          , startDate = ( 2016, Sep, 15 )
          , endDate = ( 2016, Sep, 17 )
          , location = "St. Louis, MO"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, General, FunctionalProgramming ]
          }
        , { name = "WindyCityRails"
          , link = "https://www.windycityrails.com/"
          , startDate = ( 2016, Sep, 15 )
          , endDate = ( 2016, Sep, 16 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 31 )
          , tags = [ English, Developers, USA, Ruby, Rails ]
          }
        , { name = "WindyCityThings"
          , link = "https://www.windycitythings.com/"
          , startDate = ( 2016, Jun, 23 )
          , endDate = ( 2016, Jun, 24 )
          , location = "Chicago, IL"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Apr, 15 )
          , tags = [ English, Developers, USA, InternetOfThings, RaspberryPi, Arduino ]
          }
        , { name = "CppCon"
          , link = "http://cppcon.org/"
          , startDate = ( 2016, Sep, 18 )
          , endDate = ( 2016, Sep, 23 )
          , location = "Bellevue, WA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, May, 22 )
          , tags = [ English, Developers, USA, CPlusPlus ]
          }
        , { name = "International Conference on Functional Programming"
          , link = "http://conf.researchr.org/home/icfp-2016"
          , startDate = ( 2016, Sep, 18 )
          , endDate = ( 2016, Sep, 24 )
          , location = "Nara, Japan"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 16 )
          , tags = [ English, Developers, Japan, FunctionalProgramming, Haskell ]
          }
        , { name = "JavaOne"
          , link = "https://www.oracle.com/javaone/index.html"
          , startDate = ( 2016, Sep, 18 )
          , endDate = ( 2016, Sep, 22 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Java ]
          }
        , { name = "Generate London"
          , link = "http://www.generateconf.com/"
          , startDate = ( 2016, Sep, 21 )
          , endDate = ( 2016, Sep, 23 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, England, UX ]
          }
        , { name = "AWS Summit Rio de Janeiro"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Sep, 22 )
          , endDate = ( 2016, Sep, 22 )
          , location = "Rio de Janeiro, Brazil"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Brazil, AWS, DevOps ]
          }
        , { name = "Functional Conf"
          , link = "http://functionalconf.com/"
          , startDate = ( 2016, Sep, 22 )
          , endDate = ( 2016, Sep, 25 )
          , location = "Bangalore, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Jun, 30 )
          , tags = [ English, Developers, India, FunctionalProgramming ]
          }
        , { name = "DrupalCon Dublin"
          , link = "https://events.drupal.org/dublin2016/"
          , startDate = ( 2016, Sep, 26 )
          , endDate = ( 2016, Sep, 30 )
          , location = "Dublin, Ireland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Ireland, Drupal, PHP ]
          }
        , { name = "AngularConnect"
          , link = "http://angularconnect.com/"
          , startDate = ( 2016, Sep, 27 )
          , endDate = ( 2016, Sep, 28 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, May, 31 )
          , tags = [ English, Developers, England, JavaScript, AngularJS ]
          }
        , { name = "AWS Summit Lima"
          , link = "https://aws.amazon.com/summits/"
          , startDate = ( 2016, Sep, 28 )
          , endDate = ( 2016, Sep, 28 )
          , location = "Lima, Peru"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Peru, AWS, DevOps ]
          }
        , { name = "Jazoon TechDays"
          , link = "http://jazoon.com/"
          , startDate = ( 2016, Sep, 30 )
          , endDate = ( 2016, Sep, 30 )
          , location = "Zurich, Switzerland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Switzerland, JavaScript, AngularJS, Ember ]
          }
        , { name = "An Event Apart Orlando"
          , link = "http://aneventapart.com/event/orlando-special-edition-2016"
          , startDate = ( 2016, Oct, 3 )
          , endDate = ( 2016, Oct, 5 )
          , location = "Orlando, FL"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, UX ]
          }
        , { name = "GOTO Copenhagen"
          , link = "http://gotocon.com/cph-2015/"
          , startDate = ( 2016, Oct, 3 )
          , endDate = ( 2016, Oct, 6 )
          , location = "Copenhagen, Denmark"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Denmark, General ]
          }
        , { name = "LoopConf"
          , link = "https://loopconf.com/"
          , startDate = ( 2016, Oct, 5 )
          , endDate = ( 2016, Oct, 7 )
          , location = "Fort Lauderdale, FL"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Mar, 15 )
          , tags = [ English, Developers, USA, PHP, WordPress ]
          }
        , { name = "dotGo"
          , link = "http://2016.dotgo.eu/"
          , startDate = ( 2016, Oct, 10 )
          , endDate = ( 2016, Oct, 10 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, France, Go ]
          }
        , { name = "GOTO London"
          , link = "http://gotocon.com/"
          , startDate = ( 2016, Oct, 12 )
          , endDate = ( 2016, Oct, 14 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, General ]
          }
        , { name = "Rails Remote Conf"
          , link = "https://allremoteconfs.com/rails-2016"
          , startDate = ( 2016, Oct, 12 )
          , endDate = ( 2016, Oct, 14 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Sep, 10 )
          , tags = [ English, Developers, Remote, Rails, Ruby ]
          }
        , { name = "CocoaLove"
          , link = "http://cocoalove.org/"
          , startDate = ( 2016, Oct, 14 )
          , endDate = ( 2016, Oct, 16 )
          , location = "Philadelphia, PA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Cocoa, IOS ]
          }
        , { name = "CSS Dev Conf"
          , link = "http://2016.cssdevconf.com/"
          , startDate = ( 2016, Oct, 17 )
          , endDate = ( 2016, Oct, 19 )
          , location = "San Antonio, TX"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, CSS ]
          }
        , { name = "Full Stack Toronto"
          , link = "https://fsto.co/"
          , startDate = ( 2016, Oct, 17 )
          , endDate = ( 2016, Oct, 18 )
          , location = "Toronto, Canada"
          , cfpStartDate = Just ( 2016, Jan, 1 )
          , cfpEndDate = Just ( 2016, Jul, 31 )
          , tags = [ English, Designers, Developers, Canada, General ]
          }
        , { name = "ConnectJS"
          , link = "http://connect-js.com/"
          , startDate = ( 2016, Oct, 21 )
          , endDate = ( 2016, Oct, 22 )
          , location = "Atlanta, GA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, JavaScript ]
          }
        , { name = "Codemotion Berlin"
          , link = "http://berlin2015.codemotionworld.com/"
          , startDate = ( 2016, Oct, 24 )
          , endDate = ( 2016, Oct, 25 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Germany, General ]
          }
        , { name = "ng-europe"
          , link = "https://ngeurope.org/"
          , startDate = ( 2016, Oct, 25 )
          , endDate = ( 2016, Oct, 26 )
          , location = "Paris, France"
          , cfpStartDate = Just ( 2016, Feb, 17 )
          , cfpEndDate = Just ( 2016, Mar, 31 )
          , tags = [ English, Developers, France, JavaScript, AngularJS ]
          }
        , { name = "Smashing Conf Barcelona"
          , link = "http://smashingconf.com/"
          , startDate = ( 2016, Oct, 25 )
          , endDate = ( 2016, Oct, 26 )
          , location = "Barcelona, Spain"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Spain, UX ]
          }
        , { name = "Spark Summit Europe"
          , link = "https://spark-summit.org/"
          , startDate = ( 2016, Oct, 25 )
          , endDate = ( 2016, Oct, 27 )
          , location = "Brussels, Belgium"
          , cfpStartDate = Just ( 2016, Jun, 1 )
          , cfpEndDate = Just ( 2016, Jul, 1 )
          , tags = [ English, Developers, Belgium, BigData ]
          }
        , { name = "An Event Apart San Francisco"
          , link = "http://aneventapart.com/event/san-francisco-2016"
          , startDate = ( 2016, Oct, 31 )
          , endDate = ( 2016, Nov, 2 )
          , location = "San Francisco, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, USA, UX ]
          }
        , { name = "CocoaConf San Jose"
          , link = "http://cocoaconf.com/sanjose-2016/home"
          , startDate = ( 2016, Nov, 4 )
          , endDate = ( 2016, Nov, 5 )
          , location = "San Jose, CA"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, IOS, Cocoa ]
          }
        , { name = "Beyond Tellerrand"
          , link = "http://beyondtellerrand.com/events/berlin-2016"
          , startDate = ( 2016, Nov, 7 )
          , endDate = ( 2016, Nov, 9 )
          , location = "Berlin, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, Germany, UX, General ]
          }
        , { name = "Devops Remote Conf"
          , link = "https://allremoteconfs.com/devops-2016"
          , startDate = ( 2016, Nov, 9 )
          , endDate = ( 2016, Nov, 11 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Oct, 8 )
          , tags = [ English, Developers, Remote, DevOps ]
          }
        , { name = "droidconIN"
          , link = "https://droidcon.in/2016/"
          , startDate = ( 2016, Nov, 10 )
          , endDate = ( 2016, Nov, 11 )
          , location = "Bangalore, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Aug, 10 )
          , tags = [ English, Developers, India, Android, Mobile ]
          }
        , { name = "RubyConf"
          , link = "http://rubyconf.org/"
          , startDate = ( 2016, Nov, 10 )
          , endDate = ( 2016, Nov, 12 )
          , location = "Cincinnati, OH"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Ruby ]
          }
        , { name = "BuildStuff"
          , link = "http://buildstuff.lt/"
          , startDate = ( 2016, Nov, 16 )
          , endDate = ( 2016, Nov, 20 )
          , location = "Vilnius, Lithuania"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Lithuania, General ]
          }
        , { name = "Frontier Conf"
          , link = "https://www.frontierconf.com/"
          , startDate = ( 2016, Nov, 16 )
          , endDate = ( 2016, Nov, 16 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, England, CSS, UX ]
          }
        , { name = "Generate Bangalore"
          , link = "http://www.generateconf.com/"
          , startDate = ( 2016, Nov, 25 )
          , endDate = ( 2016, Nov, 25 )
          , location = "Bangalore, India"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, India, UX ]
          }
        , { name = "AWS re:Invent"
          , link = "https://reinvent.awsevents.com/"
          , startDate = ( 2016, Nov, 28 )
          , endDate = ( 2016, Dec, 2 )
          , location = "Las Vegas, NV"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, AWS, Cloud ]
          }
        , { name = "JS Kongress Munich"
          , link = "http://js-kongress.de/"
          , startDate = ( 2016, Nov, 28 )
          , endDate = ( 2016, Nov, 29 )
          , location = "Munich, Germany"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, May, 15 )
          , tags = [ English, Developers, Germany, JavaScript ]
          }
        , { name = "CSSConf AU"
          , link = "http://2016.cssconf.com.au/"
          , startDate = ( 2016, Nov, 30 )
          , endDate = ( 2016, Nov, 30 )
          , location = "Melbourne, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Australia, CSS ]
          }
        , { name = "JSConf AU"
          , link = "http://jsconfau.com/"
          , startDate = ( 2016, Dec, 1 )
          , endDate = ( 2016, Dec, 1 )
          , location = "Melbourne, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Australia, JavaScript ]
          }
        , { name = "Clojure eXchange"
          , link = "https://skillsmatter.com/conferences/7430-clojure-exchange-2016"
          , startDate = ( 2016, Dec, 1 )
          , endDate = ( 2016, Dec, 2 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, England, Clojure, FunctionalProgramming ]
          }
        , { name = "Decompress"
          , link = "http://2016.cssconf.com.au/"
          , startDate = ( 2016, Dec, 2 )
          , endDate = ( 2016, Dec, 2 )
          , location = "Melbourne, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, Australia, General ]
          }
        , { name = "dotCSS"
          , link = "http://www.dotcss.io/"
          , startDate = ( 2016, Dec, 2 )
          , endDate = ( 2016, Dec, 2 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, France, CSS ]
          }
        , { name = "dotJS"
          , link = "http://www.dotjs.io/"
          , startDate = ( 2016, Dec, 5 )
          , endDate = ( 2016, Dec, 5 )
          , location = "Paris, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, France, JavaScript ]
          }
        , { name = "NoSQL Remote Conf"
          , link = "https://allremoteconfs.com/nosql-2016"
          , startDate = ( 2016, Dec, 7 )
          , endDate = ( 2016, Dec, 9 )
          , location = "Remote"
          , cfpStartDate = Nothing
          , cfpEndDate = Just ( 2016, Nov, 5 )
          , tags = [ English, Developers, Remote, NoSQL ]
          }
        , { name = "Midwest.io"
          , link = "http://www.midwest.io/"
          , startDate = ( 2016, Aug, 22 )
          , endDate = ( 2016, Aug, 23 )
          , location = "Kansas City, MO"
          , cfpStartDate = Nothing
          , cfpEndDate = Just (2016, Apr, 30)
          , tags = [ English, Developers, USA, General]
          }
        , { name = "ServerlessConf"
          , link = "http://serverlessconf.io/"
          , startDate = ( 2016, May, 26 )
          , endDate = ( 2016, May, 27 )
          , location = "New York, NY"
          , cfpStartDate = Nothing
          , cfpEndDate = Just (2016, Apr, 26)
          , tags = [ English, Developers, USA, General, AWS, Cloud, Scalability]
          }
        , { name = "AgileIndy"
          , link = "http://agileindy.org/"
          , startDate = ( 2016, Apr, 12 )
          , endDate = ( 2016, Apr,  12)
          , location = "Indianapolis, IN"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, USA, Agile]
          }
        , { name = "RSJS"
          , link = "http://rsjs.org/2016/"
          , startDate = ( 2016, Apr, 23 )
          , endDate = ( 2016, Apr, 23 )
          , location = "Porto Alegre, Brazil"
          , cfpStartDate = Nothing
          , cfpEndDate = Just (2016, Mar, 31)
          , tags = [ Portuguese, Developers, Brazil, JavaScript]
          }
        , { name = "Frontinsampa"
          , link = "http://frontinsampa.com.br/"
          , startDate = ( 2016, Jul, 2 )
          , endDate = ( 2016, Jul, 2 )
          , location = "Sao Paulo, Brazil"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ Portuguese, Developers, Brazil, JavaScript]
          }
        , { name = ".NET Fringe"
          , link = "http://dotnetfringe.org/"
          , startDate = ( 2016, Jul, 10 )
          , endDate = ( 2016, Jul, 12 )
          , location = "Portland, OR"
          , cfpStartDate = Nothing
          , cfpEndDate = Just (2016, Apr, 30)
          , tags = [ English, Developers, USA, OpenSource, DotNet ]
          }
        , { name = "Úll"
          , link = "http://2016.ull.ie/"
          , startDate = ( 2016, Nov, 1 )
          , endDate = ( 2016, Nov, 2 )
          , location = "Killarney, Ireland"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Ireland, IOS]
          }
        , { name = "Nantes FP Day"
          , link = "http://fpday.org/"
          , startDate = ( 2016, Mar, 26 )
          , endDate = ( 2016, Mar,  26)
          , location = "Nantes, France"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, French, Developers, France, FunctionalProgramming]
          }
        , { name = "HalfStack"
          , link = "http://halfstackconf.com/"
          , startDate = ( 2016, Nov, 18 )
          , endDate = ( 2016, Nov, 18 )
          , location = "London, England"
          , cfpStartDate = Nothing
          , cfpEndDate = Just (2016, Oct, 18)
          , tags = [ English, Developers, England, JavaScript]
          }
        , { name = "Front Conference"
          , link = "https://frontutah.com/"
          , startDate = ( 2016, May,12  )
          , endDate = ( 2016, May,13  )
          , location = "Salt Lake City, UT"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, USA, UX ]
          }
        , { name = "Port80"
          , link = "http://port80events.co.uk/"
          , startDate = ( 2016, May, 20 )
          , endDate = ( 2016,May , 20 )
          , location = "Newport, Wales"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Designers, Developers, Wales, UX, Web ]
          }
        , { name = "Code 2016 Sydney"
          , link = "http://www.webdirections.org/code16/"
          , startDate = ( 2016, Jul, 28  )
          , endDate = ( 2016, Jul, 29 )
          , location = "Sydney, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Australia, JavaScript]
          }
        , { name = "Code 2016 Melbourne"
          , link = "http://www.webdirections.org/code16/"
          , startDate = ( 2016, Aug, 1  )
          , endDate = ( 2016, Aug, 2 )
          , location = "Melbourne, Australia"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Australia, JavaScript]
          }
        , { name = "CascadiaFest"
          , link = "http://2016.cascadiafest.org/"
          , startDate = ( 2016, Aug, 3 )
          , endDate = ( 2016, Aug, 5 )
          , location = "Semiahmoo, WA"
          , cfpStartDate = Nothing
          , cfpEndDate = Just (2016, Apr, 11)
          , tags = [ English, Designers, Developers, USA, CSS, JavaScript, Web, NodeJS]
          }
        , { name = "React Amsterdam"
          , link = "http://react-amsterdam.com/"
          , startDate = ( 2016, Apr, 16 )
          , endDate = ( 2016, Apr, 16 )
          , location = "Amsterdam, Netherlands"
          , cfpStartDate = Nothing
          , cfpEndDate = Nothing
          , tags = [ English, Developers, Netherlands, JavaScript, React]
          }
        ]
  , currentDate = ( 2016, Jan, 1 )
  , tags =
      [ { sectionName = "Conference Language"
        , tags =
            [ FilteredTag.init English "English"
            , FilteredTag.init French "French"
            , FilteredTag.init German "German"
            , FilteredTag.init Italian "Italian"
            , FilteredTag.init Japanese "Japanese"
            , FilteredTag.init Norwegian "Norwegian"
            , FilteredTag.init Polish "Polish"
            , FilteredTag.init Portuguese "Portuguese"
            , FilteredTag.init Russian "Russian"
            , FilteredTag.init Spanish "Spanish"
            , FilteredTag.init Turkish "Turkish"
            ]
        }
      , { sectionName = "Audience"
        , tags =
            [ FilteredTag.init Designers "Designers"
            , FilteredTag.init Developers "Developers"
            ]
        }
      , { sectionName = "Programming Languages/Technologies"
        , tags =
            [ FilteredTag.init Android "Android"
            , FilteredTag.init AngularJS "AngularJS"
            , FilteredTag.init Arduino "Arduino"
            , FilteredTag.init AWS "AWS"
            , FilteredTag.init CPlusPlus "C++"
            , FilteredTag.init CSS "CSS"
            , FilteredTag.init Chef "Chef"
            , FilteredTag.init Clojure "Clojure"
            , FilteredTag.init Cocoa "Cocoa"
            , FilteredTag.init CycleJS "CycleJS"
            , FilteredTag.init Docker "Docker"
            , FilteredTag.init Drupal "Drupal"
            , FilteredTag.init DotNet ".NET"
            , FilteredTag.init Elasticsearch "Elasticsearch"
            , FilteredTag.init Elixir "Elixir"
            , FilteredTag.init Ember "Ember"
            , FilteredTag.init Erlang "Erlang"
            , FilteredTag.init FSharp "F#"
            , FilteredTag.init Git "Git"
            , FilteredTag.init Go "Go"
            , FilteredTag.init Gradle "Gradle"
            , FilteredTag.init Grails "Grails"
            , FilteredTag.init Groovy "Groovy"
            , FilteredTag.init Hadoop "Hadoop"
            , FilteredTag.init Haskell "Haskell"
            , FilteredTag.init IOS "iOS"
            , FilteredTag.init Java "Java"
            , FilteredTag.init JavaScript "JavaScript"
            , FilteredTag.init Logstash "Logstash"
            , FilteredTag.init Lisp "Lisp"
            , FilteredTag.init MongoDB "MongoDB"
            , FilteredTag.init NodeJS "NodeJS"
            , FilteredTag.init OCaml "OCaml"
            , FilteredTag.init PhoneGap "PhoneGap"
            , FilteredTag.init PHP "PHP"
            , FilteredTag.init PostgreSQL "PostgreSQL"
            , FilteredTag.init PureScript "PureScript"
            , FilteredTag.init Python "Python"
            , FilteredTag.init Rails "Rails"
            , FilteredTag.init RaspberryPi "RaspberryPi"
            , FilteredTag.init React "React"
            , FilteredTag.init Ruby "Ruby"
            , FilteredTag.init SML "SML"
            , FilteredTag.init Scala "Scala"
            , FilteredTag.init SVG "SVG"
            , FilteredTag.init Swift "Swift"
            , FilteredTag.init WordPress "WordPress"
            ]
        }
      , { sectionName = "Topics"
        , tags =
            [ FilteredTag.init Agile "Agile"
            , FilteredTag.init BigData "Big Data"
            , FilteredTag.init Cloud "Cloud"
            , FilteredTag.init Communications "Communications"
            , FilteredTag.init DataVisualization "DataVisualization"
            , FilteredTag.init DevOps "DevOps"
            , FilteredTag.init Diversity "Diversity"
            , FilteredTag.init FunctionalProgramming "Functional Programming"
            , FilteredTag.init General "General"
            , FilteredTag.init InternetOfThings "Internet of Things"
            , FilteredTag.init Microservices "Microservices"
            , FilteredTag.init Mobile "Mobile"
            , FilteredTag.init NoSQL "NoSQL"
            , FilteredTag.init OpenSource "Open Source"
            , FilteredTag.init ProgressiveEnhancement "Progressive Enhancement"
            , FilteredTag.init Robotics "Robotics"
            , FilteredTag.init Scalability "Scalability"
            , FilteredTag.init Security "Security"
            , FilteredTag.init SoftSkills "Soft Skills"
            , FilteredTag.init SoftwareCraftsmanship "Software Craftsmanship"
            , FilteredTag.init Testing "Testing"
            , FilteredTag.init UX "UX"
            , FilteredTag.init Web "Web"
            ]
        }
      , { sectionName = "Locations"
        , tags =
            [ FilteredTag.init Argentina "Argentina"
            , FilteredTag.init Australia "Australia"
            , FilteredTag.init Belarus "Belarus"
            , FilteredTag.init Belgium "Belgium"
            , FilteredTag.init Brazil "Brazil"
            , FilteredTag.init Bulgaria "Bulgaria"
            , FilteredTag.init Canada "Canada"
            , FilteredTag.init Chile "Chile"
            , FilteredTag.init China "China"
            , FilteredTag.init Colombia "Colombia"
            , FilteredTag.init Croatia "Croatia"
            , FilteredTag.init CzechRepublic "Czech Republic"
            , FilteredTag.init Denmark "Denmark"
            , FilteredTag.init England "England"
            , FilteredTag.init Finland "Finland"
            , FilteredTag.init France "France"
            , FilteredTag.init Germany "Germany"
            , FilteredTag.init Hungary "Hungary"
            , FilteredTag.init Iceland "Iceland"
            , FilteredTag.init India "India"
            , FilteredTag.init Ireland "Ireland"
            , FilteredTag.init Israel "Israel"
            , FilteredTag.init Italy "Italy"
            , FilteredTag.init Japan "Japan"
            , FilteredTag.init Latvia "Latvia"
            , FilteredTag.init Lebanon "Lebanon"
            , FilteredTag.init Lithuania "Lithuania"
            , FilteredTag.init Malaysia "Malaysia"
            , FilteredTag.init Mexico "Mexico"
            , FilteredTag.init Netherlands "Netherlands"
            , FilteredTag.init NewZealand "New Zealand"
            , FilteredTag.init Norway "Norway"
            , FilteredTag.init Peru "Peru"
            , FilteredTag.init Philippines "Philippines"
            , FilteredTag.init Poland "Poland"
            , FilteredTag.init Portugal "Portugal"
            , FilteredTag.init Remote "Remote"
            , FilteredTag.init Romania "Romania"
            , FilteredTag.init Russia "Russia"
            , FilteredTag.init Scotland "Scotland"
            , FilteredTag.init Singapore "Singapore"
            , FilteredTag.init SouthAfrica "South Africa"
            , FilteredTag.init SouthKorea "South Korea"
            , FilteredTag.init Spain "Spain"
            , FilteredTag.init Sweden "Sweden"
            , FilteredTag.init Switzerland "Switzerland"
            , FilteredTag.init Taiwan "Taiwan"
            , FilteredTag.init Tunisia "Tunisia"
            , FilteredTag.init Turkey "Turkey"
            , FilteredTag.init UAE "UAE"
            , FilteredTag.init USA "USA"
            , FilteredTag.init Ukraine "Ukraine"
            , FilteredTag.init Uruguay "Uruguay"
            , FilteredTag.init Wales "Wales"
            ]
        }
      ]
  , includePastEvents = False
  }
