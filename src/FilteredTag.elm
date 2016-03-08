module FilteredTag (init, Model, Action, update, view, initializeIncludedTag, isIncluded, exclude) where

import FilteredTagInternal exposing (..)
import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events
import Tag exposing (Tag)


-- Model


type alias State =
  FilteredTagInternal.State


type alias Model =
  { tag : Tag
  , state : State
  , display : String
  }


init : Tag -> String -> Model
init tag display =
  { tag = tag
  , state = Excluded
  , display = display
  }



-- Update


type alias Action =
  FilteredTagInternal.Action


update : Action -> Model -> Model
update action model =
  case action of
    Include ->
      { model | state = Included }

    Exclude ->
      { model | state = Excluded }



-- Public functions


initializeIncludedTag : List String -> Model -> Model
initializeIncludedTag includedTags model =
  if List.member (toString model.tag) includedTags then
    update Include model
  else
    model


exclude : Model -> Model
exclude model =
  update Exclude model


isIncluded : Model -> Bool
isIncluded model =
  case model.state of
    Included ->
      True

    Excluded ->
      False



-- View


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    ( tagString, tagClass, clickAction ) =
      case model.state of
        Included ->
          ( "- " ++ model.display, "included", Exclude )

        Excluded ->
          ( "+ " ++ model.display, "excluded", Include )
  in
    Html.button
      [ class tagClass
      , Html.Events.onClick address clickAction
      ]
      [ text tagString ]
