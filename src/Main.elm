module Confs where

import Html exposing (text)
import Html.Attributes exposing (href)
import Date
import DateFormatter exposing (parseDate)

type alias Model =
  { conferences : List Conference
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

view : Model -> Html.Html
view model =
  Html.div []
  [ Html.header [] [text "Confs.io"]
  , Html.table [] <| conferencesView model
  ]

conferencesView : Model -> List Html.Html
conferencesView model =
  List.map conferenceView model.conferences


conferenceView : Conference -> Html.Html
conferenceView conference =
  Html.tr
    []
    [ Html.td
        []
        [Html.a [href conference.link] [text conference.name]]
    , Html.td
        []
        [text <| DateFormatter.formatRange conference.startDate conference.endDate]
    , Html.td
        []
        [text conference.location]
    ]

main : Html.Html
main =
  view initialState

initialState : Model
initialState =
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
  }
