module Conferences where

import Date
import DateFormatter exposing (parseDate)

type alias Model =
  { conferences : List Conference
  , tags : List FilteredTag
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
  Ruby

type FilteredTag =
  Included Tag
  | Excluded Tag

type Action = Include Tag

update : Action -> Model -> Model
update action model =
  model

list : Model
list =
  { conferences =
      [
        { name = "NDC London Workshops"
        , link = "http://ndc-london.com/"
        , startDate = parseDate "1/11/2016"
        , endDate = parseDate "1/12/2016"
        , location = "London, UK"
        , tags = []
        }
      , { name = "NDC London"
        , link = "http://ndc-london.com/"
        , startDate = parseDate "1/13/2016"
        , endDate = parseDate "1/15/2016"
        , location = "London, UK"
        , tags = []
        }
      ]
  , tags =
    [ Included Ruby
    ]
  }
