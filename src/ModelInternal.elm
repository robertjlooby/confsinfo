module ModelInternal (..) where

import Conference
import DaTuple exposing (DaTuple, compare')
import FilteredTagSection
import GenericSet as GSet
import Tag exposing (Tag)
import Time exposing (Time)


-- Model


type alias Model =
  { conferences : GSet.GenericSet Conference.Model
  , currentDate : DaTuple
  , includePastEvents : Bool
  , tags : List FilteredTagSection.Model
  }



-- Update


type Action
  = UpdateTag FilteredTagSection.Action
  | IncludePastEvents Bool
  | SetCurrentDate (Maybe Time)


conferencesToShow : Model -> List Conference.Model
conferencesToShow model =
  let
    isInFuture conference =
      compare' model.currentDate conference.startDate /= GT

    confsToFilterOnTags =
      if model.includePastEvents then
        GSet.toList model.conferences
      else
        List.filter isInFuture <| GSet.toList model.conferences
  in
    List.filter (Conference.shouldShow <| includedTags model) confsToFilterOnTags



-- Public functions


includedTags : Model -> List Tag
includedTags model =
  List.map FilteredTagSection.includedTags model.tags
    |> List.concat
