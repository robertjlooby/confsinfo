module InitialData exposing (model)

import Conference
import FilteredTag
import Model
import Tag exposing (..)
import Time.Date as Date


model : Model.Model
model =
    { conferences =
        [ { name = "@Swift"
          , link = "http://atswift.io/index-en.html"
          , startDate = Date.date 2016 1 10
          , endDate = Date.date 2016 1 10
          , location = "Beijing, China"
          , cfpStatus = Conference.Closed
          }
        , { name = "NDC London Workshops"
          , link = "http://ndc-london.com/"
          , startDate = Date.date 2016 1 11
          , endDate = Date.date 2016 1 12
          , location = "London, England"
          , cfpStatus = Conference.Closed
          }
        ]
    , audiences =
        { sectionName = "Audience"
        , tags =
            [ FilteredTag.init (Audience "Designers")
            , FilteredTag.init (Audience "Developers")
            ]
        }
    , languages =
        { sectionName = "Conference Language"
        , tags =
            [ FilteredTag.init (Language "English")
            , FilteredTag.init (Language "French")
            , FilteredTag.init (Language "German")
            , FilteredTag.init (Language "Italian")
            , FilteredTag.init (Language "Japanese")
            , FilteredTag.init (Language "Norwegian")
            , FilteredTag.init (Language "Polish")
            , FilteredTag.init (Language "Portuguese")
            , FilteredTag.init (Language "Russian")
            , FilteredTag.init (Language "Spanish")
            , FilteredTag.init (Language "Turkish")
            ]
        }
    , locations =
        { sectionName = "Locations"
        , tags =
            [ FilteredTag.init (Location "Argentina")
            , FilteredTag.init (Location "Australia")
            , FilteredTag.init (Location "Belarus")
            , FilteredTag.init (Location "Belgium")
            , FilteredTag.init (Location "Brazil")
            , FilteredTag.init (Location "Bulgaria")
            , FilteredTag.init (Location "Canada")
            , FilteredTag.init (Location "Chile")
            , FilteredTag.init (Location "China")
            , FilteredTag.init (Location "Colombia")
            , FilteredTag.init (Location "Croatia")
            , FilteredTag.init (Location "Czech Republic")
            , FilteredTag.init (Location "Denmark")
            , FilteredTag.init (Location "England")
            , FilteredTag.init (Location "Finland")
            , FilteredTag.init (Location "France")
            , FilteredTag.init (Location "Germany")
            , FilteredTag.init (Location "Hungary")
            , FilteredTag.init (Location "Iceland")
            , FilteredTag.init (Location "India")
            , FilteredTag.init (Location "Ireland")
            , FilteredTag.init (Location "Israel")
            , FilteredTag.init (Location "Italy")
            , FilteredTag.init (Location "Japan")
            , FilteredTag.init (Location "Latvia")
            , FilteredTag.init (Location "Lebanon")
            , FilteredTag.init (Location "Lithuania")
            , FilteredTag.init (Location "Malaysia")
            , FilteredTag.init (Location "Mexico")
            , FilteredTag.init (Location "Netherlands")
            , FilteredTag.init (Location "New Zealand")
            , FilteredTag.init (Location "Norway")
            , FilteredTag.init (Location "Peru")
            , FilteredTag.init (Location "Philippines")
            , FilteredTag.init (Location "Poland")
            , FilteredTag.init (Location "Portugal")
            , FilteredTag.init (Location "Remote")
            , FilteredTag.init (Location "Romania")
            , FilteredTag.init (Location "Russia")
            , FilteredTag.init (Location "Scotland")
            , FilteredTag.init (Location "Singapore")
            , FilteredTag.init (Location "South Africa")
            , FilteredTag.init (Location "South Korea")
            , FilteredTag.init (Location "Spain")
            , FilteredTag.init (Location "Sweden")
            , FilteredTag.init (Location "Switzerland")
            , FilteredTag.init (Location "Taiwan")
            , FilteredTag.init (Location "Tunisia")
            , FilteredTag.init (Location "Turkey")
            , FilteredTag.init (Location "UAE")
            , FilteredTag.init (Location "USA")
            , FilteredTag.init (Location "Ukraine")
            , FilteredTag.init (Location "Uruguay")
            , FilteredTag.init (Location "Wales")
            ]
        }
    , topics =
        { sectionName = "Topics"
        , tags =
            [ FilteredTag.init (Topic ".Net")
            , FilteredTag.init (Topic "AWS")
            , FilteredTag.init (Topic "Agile")
            , FilteredTag.init (Topic "Android")
            , FilteredTag.init (Topic "AngularJS")
            , FilteredTag.init (Topic "Arduino")
            , FilteredTag.init (Topic "Big Data")
            , FilteredTag.init (Topic "C++")
            , FilteredTag.init (Topic "CSS")
            , FilteredTag.init (Topic "Chef")
            , FilteredTag.init (Topic "Clojure")
            , FilteredTag.init (Topic "Cloud")
            , FilteredTag.init (Topic "Cocoa")
            , FilteredTag.init (Topic "Communications")
            , FilteredTag.init (Topic "CycleJS")
            , FilteredTag.init (Topic "DataVisualization")
            , FilteredTag.init (Topic "DevOps")
            , FilteredTag.init (Topic "Diversity")
            , FilteredTag.init (Topic "Docker")
            , FilteredTag.init (Topic "Drupal")
            , FilteredTag.init (Topic "Elasticsearch")
            , FilteredTag.init (Topic "Elixir")
            , FilteredTag.init (Topic "Ember")
            , FilteredTag.init (Topic "Erlang")
            , FilteredTag.init (Topic "F#")
            , FilteredTag.init (Topic "Functional Programming")
            , FilteredTag.init (Topic "General")
            , FilteredTag.init (Topic "Git")
            , FilteredTag.init (Topic "Go")
            , FilteredTag.init (Topic "Gradle")
            , FilteredTag.init (Topic "Grails")
            , FilteredTag.init (Topic "Groovy")
            , FilteredTag.init (Topic "Hadoop")
            , FilteredTag.init (Topic "Haskell")
            , FilteredTag.init (Topic "IOS")
            , FilteredTag.init (Topic "Internet Of Things")
            , FilteredTag.init (Topic "Java")
            , FilteredTag.init (Topic "JavaScript")
            , FilteredTag.init (Topic "Lisp")
            , FilteredTag.init (Topic "Logstash")
            , FilteredTag.init (Topic "Microservices")
            , FilteredTag.init (Topic "Mobile")
            , FilteredTag.init (Topic "MongoDB")
            , FilteredTag.init (Topic "NoSQL")
            , FilteredTag.init (Topic "NodeJS")
            , FilteredTag.init (Topic "OCaml")
            , FilteredTag.init (Topic "Open Source")
            , FilteredTag.init (Topic "PHP")
            , FilteredTag.init (Topic "PhoneGap")
            , FilteredTag.init (Topic "PostgreSQL")
            , FilteredTag.init (Topic "Progressive Enhancement")
            , FilteredTag.init (Topic "PureScript")
            , FilteredTag.init (Topic "Python")
            , FilteredTag.init (Topic "Rails")
            , FilteredTag.init (Topic "RaspberryPi")
            , FilteredTag.init (Topic "React")
            , FilteredTag.init (Topic "Robotics")
            , FilteredTag.init (Topic "Ruby")
            , FilteredTag.init (Topic "SML")
            , FilteredTag.init (Topic "SVG")
            , FilteredTag.init (Topic "Scala")
            , FilteredTag.init (Topic "Scalability")
            , FilteredTag.init (Topic "Security")
            , FilteredTag.init (Topic "Soft Skills")
            , FilteredTag.init (Topic "Software Craftsmanship")
            , FilteredTag.init (Topic "Swift")
            , FilteredTag.init (Topic "Testing")
            , FilteredTag.init (Topic "UX")
            , FilteredTag.init (Topic "Web")
            , FilteredTag.init (Topic "WordPress")
            ]
        }
    , includePastEvents = False
    }
