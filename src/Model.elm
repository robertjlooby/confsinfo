module Model exposing (Model, Msg(..), conferencesToShow, init, update, urlUpdate, initializeIncludedTags, includedTags, setCurrentDate, view)

import Conference
import Date
import DaTuple exposing (DaTuple, compare')
import GenericSet as GSet
import FilteredTagSection
import Html exposing (text)
import Html.App as App
import Html.Attributes exposing (class, href)
import Html.Events
import Navigation exposing (modifyUrl)
import Route.QueryString exposing (QueryString, add, all, empty, render, parse)
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


init : Model -> QueryString -> ( Model, Cmd Msg )
init initialModel queryString =
    let
        tags =
            all "tag" queryString

        model =
            initializeIncludedTags tags initialModel
    in
        ( model, initializeDate )



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

                cmd =
                    List.foldr (toString >> add "tag") empty (includedTags newModel)
                        |> render
                        |> modifyUrl
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


urlUpdate : QueryString -> Model -> ( Model, Cmd Msg )
urlUpdate =
    (\_ model -> ( model, Cmd.none ))


initializeIncludedTags : List String -> Model -> Model
initializeIncludedTags includedTags model =
    { model | tags = List.map (FilteredTagSection.initializeIncludedTags includedTags) model.tags }


initializeDate : Cmd Msg
initializeDate =
    Task.perform (\_ -> setCurrentDate Nothing)
        (\time -> setCurrentDate <| Just time)
        Time.now


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
