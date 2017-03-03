module Model exposing (Model, Msg(..), conferencesToShow, init, update, urlUpdate, initializeIncludedTags, includedTags, setCurrentDate, view)

import Conference
import Date
import DaTuple exposing (DaTuple, compareDaTuples)
import FilteredTagSection
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Html.Events
import Navigation exposing (modifyUrl)
import QueryString exposing (QueryString, add, all, empty, one, render, string, parse)
import Tag exposing (Tag)
import Task exposing (Task)
import Time exposing (Time)


-- Model


type alias Model =
    { conferences : List Conference.Model
    , currentDate : DaTuple
    , includePastEvents : Bool
    , tags : List FilteredTagSection.Model
    }


init : Model -> Navigation.Location -> ( Model, Cmd Msg )
init initialModel { search } =
    let
        queryString =
            parse search

        tags =
            all "tag" queryString

        includePastEvents =
            one string "includePastEvents" queryString == Just "True"

        model =
            initializeIncludedTags tags initialModel
                |> update (IncludePastEvents includePastEvents)
                |> Tuple.first
    in
        ( model, initializeDate )



-- Update


type Msg
    = NoOp
    | UpdateTag FilteredTagSection.Msg
    | IncludePastEvents Bool
    | SetCurrentDate (Maybe Time)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UpdateTag action ->
            let
                newModel =
                    { model | tags = List.map (FilteredTagSection.update action) model.tags }
            in
                ( newModel, updateQueryString newModel )

        IncludePastEvents shouldIncludePastEvents ->
            let
                newModel =
                    { model | includePastEvents = shouldIncludePastEvents }
            in
                ( newModel, updateQueryString newModel )

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


updateQueryString : Model -> Cmd Msg
updateQueryString model =
    List.foldr (toString >> add "tag") empty (includedTags model)
        |> (if model.includePastEvents then
                add "includePastEvents" (toString model.includePastEvents)
            else
                identity
           )
        |> render
        |> modifyUrl


initializeIncludedTags : List String -> Model -> Model
initializeIncludedTags includedTags model =
    { model | tags = List.map (FilteredTagSection.initializeIncludedTags includedTags) model.tags }


initializeDate : Cmd Msg
initializeDate =
    Task.perform
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
    Html.div [ class "container" ] <|
        List.concat
            [ allTagsView model.tags
            , [ includePastEventsButtonView model.includePastEvents ]
            , [ Html.map UpdateTag FilteredTagSection.resetButtonView ]
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
            compareDaTuples model.currentDate conference.startDate /= GT

        confsToFilterOnTags =
            if model.includePastEvents then
                model.conferences
            else
                List.filter isInFuture model.conferences
    in
        List.filter (Conference.shouldShow <| includedTags model) confsToFilterOnTags


conferencesView : Model -> List (Html.Html Msg)
conferencesView model =
    List.map (Conference.view model.currentDate) (conferencesToShow model)


allTagsView : List FilteredTagSection.Model -> List (Html.Html Msg)
allTagsView filteredTagSections =
    List.map FilteredTagSection.view filteredTagSections
        |> List.concat
        |> List.map (Html.map UpdateTag)
