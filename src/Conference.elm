module Conference (..) where

import DateFormatter exposing (compare', DaTuple)
import Tag exposing (Tag)


type alias Conference =
  { name : String
  , link : String
  , startDate : DaTuple
  , endDate : DaTuple
  , location : String
  , cfpStartDate : Maybe DaTuple
  , cfpEndDate : Maybe DaTuple
  , tags : List Tag
  }


shouldShowConference : List Tag -> Conference -> Bool
shouldShowConference tags conference =
  List.all (\tag -> List.member tag conference.tags) tags


type CFPStatus
  = Closed
  | NotYetOpen
  | Open


cfpStatus : DaTuple -> Conference -> ( CFPStatus, Maybe DaTuple )
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
