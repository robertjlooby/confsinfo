module Model exposing (Model, Msg(..), conferencesToShow, update, initializeIncludedTags, includedTags, setCurrentDate, view)

import Conference
import Date
import DaTuple exposing (DaTuple, compare')
import GenericSet as GSet
import FilteredTagSection
import Html exposing (text)
import Html.App as App
import Html.Attributes exposing (class, href)
import Html.Events
import Tag exposing (Tag)
import Task exposing (Task)
import Time exposing (Time)


-- Model


type alias Model =
    { conferences : GSet.GenericSet Conference.Model
    , currentDate : DaTuple
    , includePastEvents : Bool
    , tags : List FilteredTagSection.Model
    }



-- Update


type Msg
    = UpdateTag FilteredTagSection.Msg
    | IncludePastEvents Bool
    | SetCurrentDate (Maybe Time)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTag action ->
            let
                newModel =
                    { model | tags = List.map (FilteredTagSection.update action) model.tags }
            in
                ( newModel, Cmd.none )

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


initializeIncludedTags : List String -> Model -> Model
initializeIncludedTags includedTags model =
    { model | tags = List.map (FilteredTagSection.initializeIncludedTags includedTags) model.tags }


setCurrentDate : Maybe Time -> Msg
setCurrentDate time =
    SetCurrentDate time



-- Public functions


includedTags : Model -> List Tag
includedTags model =
    List.map FilteredTagSection.includedTags model.tags
        |> List.concat



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


conferencesView : Model -> List (Html.Html Msg)
conferencesView model =
    List.map (Conference.view model.currentDate) (conferencesToShow model)


allTagsView : List FilteredTagSection.Model -> List (Html.Html Msg)
allTagsView filteredTagSections =
    List.map FilteredTagSection.view filteredTagSections
        |> List.concat
        |> List.map (App.map UpdateTag)
