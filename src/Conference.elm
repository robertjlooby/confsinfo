module Conference (..) where

import DateFormatter exposing (DaTuple)
import Tag exposing (Tag)


type alias Conference =
  { name : String
  , link : String
  , startDate : DaTuple
  , endDate : DaTuple
  , location : String
  , tags : List Tag
  }


shouldShowConference : List Tag -> Conference -> Bool
shouldShowConference tags conference =
  List.all (\tag -> List.member tag conference.tags) tags
