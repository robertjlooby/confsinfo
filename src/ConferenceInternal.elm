module ConferenceInternal exposing (..)

import DaTuple exposing (compare', DaTuple)
import Tag exposing (Tag)


-- Model


type alias Model =
  { name : String
  , link : String
  , startDate : DaTuple
  , endDate : DaTuple
  , location : String
  , cfpStartDate : Maybe DaTuple
  , cfpEndDate : Maybe DaTuple
  , tags : List Tag
  }


type CFPStatus
  = Closed
  | NotYetOpen
  | Open


cfpStatus : DaTuple -> Model -> ( CFPStatus, Maybe DaTuple )
cfpStatus currentDate conference =
  case Maybe.map (compare' currentDate) conference.cfpEndDate of
    Nothing ->
      ( Closed, Nothing )

    Just GT ->
      ( Closed, Nothing )

    Just _ ->
      case Maybe.map (compare' currentDate) conference.cfpStartDate of
        Just LT ->
          ( NotYetOpen, conference.cfpStartDate )

        _ ->
          ( Open, conference.cfpEndDate )
