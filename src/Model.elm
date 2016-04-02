module Model (Model, Action, update, initializeIncludedTags, includedTags, setCurrentDate, view) where

import Conference
import Date
import DaTuple exposing (DaTuple, compare')
import Effects exposing (Effects)
import FilteredTagSection
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Html.Events
import ModelInternal exposing (Action(..))
import Tag exposing (Tag)
import Task exposing (Task)
import Time exposing (Time)


-- Model


type alias Model =
  ModelInternal.Model



-- Update


type alias Action =
  ModelInternal.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    UpdateTag action ->
      ( { model | tags = List.map (FilteredTagSection.update action) model.tags }, Effects.none )

    IncludePastEvents shouldIncludePastEvents ->
      ( { model | includePastEvents = shouldIncludePastEvents }, Effects.none )

    SetCurrentDate (Just time) ->
      let
        date =
          Date.fromTime time

        daTuple =
          ( Date.year date, Date.month date, Date.day date )
      in
        ( { model | currentDate = daTuple }, Effects.none )

    SetCurrentDate Nothing ->
      ( model, Effects.none )


initializeIncludedTags : List String -> Model -> Model
initializeIncludedTags includedTags model =
  { model | tags = List.map (FilteredTagSection.initializeIncludedTags includedTags) model.tags }


setCurrentDate : Maybe Time -> Action
setCurrentDate time =
  SetCurrentDate time



-- Public functions


includedTags : Model -> List Tag
includedTags =
  ModelInternal.includedTags



-- View


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div
    [ class "container" ]
    <| List.concat
        [ allTagsView address model.tags
        , [ includePastEventsButtonView address model.includePastEvents ]
        , [ FilteredTagSection.resetButtonView (Signal.forwardTo address UpdateTag) ]
        , conferencesView model
        , [ sourceCodeLink ]
        ]


includePastEventsButtonView : Signal.Address Action -> Bool -> Html.Html
includePastEventsButtonView address includePastEvents =
  let
    label =
      "Include Past Events"

    ( buttonText, tagClass, clickAction ) =
      case includePastEvents of
        True ->
          ( "- " ++ label, "included", IncludePastEvents False )

        False ->
          ( "+ " ++ label, "excluded", IncludePastEvents True )
  in
    Html.div
      [ class "row" ]
      [ Html.button
          [ class <| "four columns offset-by-four " ++ tagClass
          , Html.Events.onClick address clickAction
          ]
          [ text buttonText ]
      ]


sourceCodeLink : Html.Html
sourceCodeLink =
  Html.div
    [ class "row" ]
    [ Html.a
        [ href "https://github.com/robertjlooby/confsinfo"
        , class "two columns offset-by-five"
        ]
        [ text "source" ]
    ]


conferencesView : Model -> List Html.Html
conferencesView model =
  List.map (Conference.view model.currentDate) (ModelInternal.conferencesToShow model)


allTagsView : Signal.Address Action -> List FilteredTagSection.Model -> List Html.Html
allTagsView address filteredTagSections =
  List.map (FilteredTagSection.view <| Signal.forwardTo address UpdateTag) filteredTagSections
    |> List.concat
