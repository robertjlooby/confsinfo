module InitialData exposing (model)

import Conference
import FilteredTag
import Model
import Tag exposing (..)


model : Model.Model
model =
    { conferences = []
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
    , tags =
        [ { sectionName = "Audience"
          , tags =
                [ FilteredTag.init (Tag "Designers")
                , FilteredTag.init (Tag "Developers")
                ]
          }
        , { sectionName = "Programming Languages/Technologies"
          , tags =
                [ FilteredTag.init (Tag "Android")
                , FilteredTag.init (Tag "AngularJS")
                , FilteredTag.init (Tag "Arduino")
                , FilteredTag.init (Tag "AWS")
                , FilteredTag.init (Tag "C++")
                , FilteredTag.init (Tag "CSS")
                , FilteredTag.init (Tag "Chef")
                , FilteredTag.init (Tag "Clojure")
                , FilteredTag.init (Tag "Cocoa")
                , FilteredTag.init (Tag "CycleJS")
                , FilteredTag.init (Tag "Docker")
                , FilteredTag.init (Tag "Drupal")
                , FilteredTag.init (Tag ".Net")
                , FilteredTag.init (Tag "Elasticsearch")
                , FilteredTag.init (Tag "Elixir")
                , FilteredTag.init (Tag "Ember")
                , FilteredTag.init (Tag "Erlang")
                , FilteredTag.init (Tag "F#")
                , FilteredTag.init (Tag "Git")
                , FilteredTag.init (Tag "Go")
                , FilteredTag.init (Tag "Gradle")
                , FilteredTag.init (Tag "Grails")
                , FilteredTag.init (Tag "Groovy")
                , FilteredTag.init (Tag "Hadoop")
                , FilteredTag.init (Tag "Haskell")
                , FilteredTag.init (Tag "IOS")
                , FilteredTag.init (Tag "Java")
                , FilteredTag.init (Tag "JavaScript")
                , FilteredTag.init (Tag "Logstash")
                , FilteredTag.init (Tag "Lisp")
                , FilteredTag.init (Tag "MongoDB")
                , FilteredTag.init (Tag "NodeJS")
                , FilteredTag.init (Tag "OCaml")
                , FilteredTag.init (Tag "PhoneGap")
                , FilteredTag.init (Tag "PHP")
                , FilteredTag.init (Tag "PostgreSQL")
                , FilteredTag.init (Tag "PureScript")
                , FilteredTag.init (Tag "Python")
                , FilteredTag.init (Tag "Rails")
                , FilteredTag.init (Tag "RaspberryPi")
                , FilteredTag.init (Tag "React")
                , FilteredTag.init (Tag "Ruby")
                , FilteredTag.init (Tag "SML")
                , FilteredTag.init (Tag "Scala")
                , FilteredTag.init (Tag "SVG")
                , FilteredTag.init (Tag "Swift")
                , FilteredTag.init (Tag "WordPress")
                ]
          }
        , { sectionName = "Topics"
          , tags =
                [ FilteredTag.init (Tag "Agile")
                , FilteredTag.init (Tag "Big Data")
                , FilteredTag.init (Tag "Cloud")
                , FilteredTag.init (Tag "Communications")
                , FilteredTag.init (Tag "DataVisualization")
                , FilteredTag.init (Tag "DevOps")
                , FilteredTag.init (Tag "Diversity")
                , FilteredTag.init (Tag "Functional Programming")
                , FilteredTag.init (Tag "General")
                , FilteredTag.init (Tag "Internet Of Things")
                , FilteredTag.init (Tag "Microservices")
                , FilteredTag.init (Tag "Mobile")
                , FilteredTag.init (Tag "NoSQL")
                , FilteredTag.init (Tag "Open Source")
                , FilteredTag.init (Tag "Progressive Enhancement")
                , FilteredTag.init (Tag "Robotics")
                , FilteredTag.init (Tag "Scalability")
                , FilteredTag.init (Tag "Security")
                , FilteredTag.init (Tag "Soft Skills")
                , FilteredTag.init (Tag "Software Craftsmanship")
                , FilteredTag.init (Tag "Testing")
                , FilteredTag.init (Tag "UX")
                , FilteredTag.init (Tag "Web")
                ]
          }
        , { sectionName = "Locations"
          , tags =
                [ FilteredTag.init (Tag "Argentina")
                , FilteredTag.init (Tag "Australia")
                , FilteredTag.init (Tag "Belarus")
                , FilteredTag.init (Tag "Belgium")
                , FilteredTag.init (Tag "Brazil")
                , FilteredTag.init (Tag "Bulgaria")
                , FilteredTag.init (Tag "Canada")
                , FilteredTag.init (Tag "Chile")
                , FilteredTag.init (Tag "China")
                , FilteredTag.init (Tag "Colombia")
                , FilteredTag.init (Tag "Croatia")
                , FilteredTag.init (Tag "Czech Republic")
                , FilteredTag.init (Tag "Denmark")
                , FilteredTag.init (Tag "England")
                , FilteredTag.init (Tag "Finland")
                , FilteredTag.init (Tag "France")
                , FilteredTag.init (Tag "Germany")
                , FilteredTag.init (Tag "Hungary")
                , FilteredTag.init (Tag "Iceland")
                , FilteredTag.init (Tag "India")
                , FilteredTag.init (Tag "Ireland")
                , FilteredTag.init (Tag "Israel")
                , FilteredTag.init (Tag "Italy")
                , FilteredTag.init (Tag "Japan")
                , FilteredTag.init (Tag "Latvia")
                , FilteredTag.init (Tag "Lebanon")
                , FilteredTag.init (Tag "Lithuania")
                , FilteredTag.init (Tag "Malaysia")
                , FilteredTag.init (Tag "Mexico")
                , FilteredTag.init (Tag "Netherlands")
                , FilteredTag.init (Tag "New Zealand")
                , FilteredTag.init (Tag "Norway")
                , FilteredTag.init (Tag "Peru")
                , FilteredTag.init (Tag "Philippines")
                , FilteredTag.init (Tag "Poland")
                , FilteredTag.init (Tag "Portugal")
                , FilteredTag.init (Tag "Remote")
                , FilteredTag.init (Tag "Romania")
                , FilteredTag.init (Tag "Russia")
                , FilteredTag.init (Tag "Scotland")
                , FilteredTag.init (Tag "Singapore")
                , FilteredTag.init (Tag "South Africa")
                , FilteredTag.init (Tag "South Korea")
                , FilteredTag.init (Tag "Spain")
                , FilteredTag.init (Tag "Sweden")
                , FilteredTag.init (Tag "Switzerland")
                , FilteredTag.init (Tag "Taiwan")
                , FilteredTag.init (Tag "Tunisia")
                , FilteredTag.init (Tag "Turkey")
                , FilteredTag.init (Tag "UAE")
                , FilteredTag.init (Tag "USA")
                , FilteredTag.init (Tag "Ukraine")
                , FilteredTag.init (Tag "Uruguay")
                , FilteredTag.init (Tag "Wales")
                ]
          }
        ]
    , includePastEvents = False
    }
