port module Model exposing (Model, Msg, update, initializeIncludedTags, includedTags, setCurrentDate, view)

import Conference
import Date
import DaTuple exposing (DaTuple, compare')
import FilteredTagSection
import Html exposing (text)
import Html.App as App
import Html.Attributes exposing (class, href)
import Html.Events
import ModelInternal exposing (Msg(..))
import Tag exposing (Tag)
import Task exposing (Task)
import Time exposing (Time)


-- Model


type alias Model =
    ModelInternal.Model



-- Update


type alias Msg =
    ModelInternal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTag action ->
            let
                newModel =
                    { model | tags = List.map (FilteredTagSection.update action) model.tags }

                cmd =
                    setStorage <| (includedTags >> List.map toString) newModel
            in
                ( newModel, cmd )

        IncludePastEvents shouldIncludePastEvents ->
            ( { model | includePastEvents = shouldIncludePastEvents }, Cmd.none )

        SetCurrentDate (Just time) ->
            let
                date =
                    Date.fromTime time

                daTuple =
                    ( Date.year date, Date.month date, Date.day date )
            in
                ( { model | currentDate = daTuple }, Cmd.none )

        SetCurrentDate Nothing ->
            ( model, Cmd.none )


port setStorage : List String -> Cmd msg


initializeIncludedTags : List String -> Model -> Model
initializeIncludedTags includedTags model =
    { model | tags = List.map (FilteredTagSection.initializeIncludedTags includedTags) model.tags }


setCurrentDate : Maybe Time -> Msg
setCurrentDate time =
    SetCurrentDate time



-- Public functions


includedTags : Model -> List Tag
includedTags =
    ModelInternal.includedTags



-- View


view : Model -> Html.Html Msg
view model =
    Html.div [ class "container" ]
        <| List.concat
            [ allTagsView model.tags
            , [ includePastEventsButtonView model.includePastEvents ]
            , [ App.map UpdateTag FilteredTagSection.resetButtonView ]
            , conferencesView model
            , [ sourceCodeLink ]
            ]


includePastEventsButtonView : Bool -> Html.Html Msg
includePastEventsButtonView includePastEvents =
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
        Html.div [ class "row" ]
            [ Html.button
                [ class <| "four columns offset-by-four " ++ tagClass
                , Html.Events.onClick clickAction
                ]
                [ text buttonText ]
            ]


sourceCodeLink : Html.Html msg
sourceCodeLink =
    Html.div [ class "row" ]
        [ Html.a
            [ href "https://github.com/robertjlooby/confsinfo"
            , class "two columns offset-by-five"
            ]
            [ text "source" ]
        ]


conferencesView : Model -> List (Html.Html Msg)
conferencesView model =
    List.map (Conference.view model.currentDate) (ModelInternal.conferencesToShow model)


allTagsView : List FilteredTagSection.Model -> List (Html.Html Msg)
allTagsView filteredTagSections =
    List.map FilteredTagSection.view filteredTagSections
        |> List.concat
        |> List.map (App.map UpdateTag)
