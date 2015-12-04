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
        , startDate = parseDate "1/11/2016"
        , endDate = parseDate "1/12/2016"
        , location = "London, UK"
        , tags = [Agile]
        }
      , { name = "NDC London"
        , link = "http://ndc-london.com/"
        , startDate = parseDate "1/13/2016"
        , endDate = parseDate "1/15/2016"
        , location = "London, UK"
        , tags = [Agile, DotNet]
        }
      ]
  , tags =
    [("Languages/Technologies",
      [ Excluded Agile
      , Excluded DotNet
      , Excluded Ruby
      ]
    )]
  }
