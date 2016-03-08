module FilteredTagSection (Model, initializeIncludedTags, includedTags, Action, update, view, resetButtonView) where

import FilteredTag
import FilteredTagSectionInternal exposing (..)
import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events
import Tag exposing (Tag)


-- Model


type alias Model =
  { sectionName : String
  , tags : List FilteredTag.Model
  }


includedTags : Model -> List Tag
includedTags model =
  List.filter FilteredTag.isIncluded model.tags
    |> List.map .tag



-- Update


type alias Action =
  FilteredTagSectionInternal.Action


update : Action -> Model -> Model
update action model =
  case action of
    UpdateTag tag tagAction ->
      let
        updateTag filteredTag =
          if filteredTag.tag == tag then
            FilteredTag.update tagAction filteredTag
          else
            filteredTag
      in
        { model | tags = List.map updateTag model.tags }

    Reset ->
      { model | tags = List.map FilteredTag.exclude model.tags }


initializeIncludedTags : List String -> Model -> Model
initializeIncludedTags includedTags model =
  { model | tags = List.map (FilteredTag.initializeIncludedTag includedTags) model.tags }



-- View


view : Signal.Address Action -> Model -> List Html.Html
view address { sectionName, tags } =
  [ Html.div
      [ class "row" ]
      [ Html.h5 [] [ text sectionName ] ]
  , Html.div
      [ class "row" ]
      <| List.map (\tag -> FilteredTag.view (Signal.forwardTo address (UpdateTag tag.tag)) tag) tags
  ]


resetButtonView : Signal.Address Action -> Html.Html
resetButtonView address =
  Html.div
    [ class "row" ]
    [ Html.button
        [ class "two columns offset-by-five"
        , Html.Events.onClick address Reset
        ]
        [ text "Reset" ]
    ]
