module Model exposing (Model, Msg(..), generateQueryString, init, update, urlUpdate, includedTopics, includedLanguages, includedLocations, includedAudiences, view)

import Conference exposing (Conference)
import FilteredTagSection exposing (FilteredTagSection)
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Html.Events
import Navigation exposing (modifyUrl)
import QueryString exposing (QueryString, add, all, empty, one, render, string, parse)
import Tag exposing (..)


-- Model


type alias Model =
    { conferences : List Conference
    , includePastEvents : Bool
    , audiences : FilteredTagSection Audience
    , languages : FilteredTagSection Language
    , locations : FilteredTagSection Location
    , topics : FilteredTagSection Topic
    }


init : Model -> { navLocation | search : String } -> ( Model, Cmd Msg )
init initialModel { search } =
    let
        queryString =
            parse search

        audiences =
            all "audience" queryString
                |> List.map Audience

        languages =
            all "language" queryString
                |> List.map Language

        locations =
            all "location" queryString
                |> List.map Location

        topics =
            all "topic" queryString
                |> List.map Topic

        includePastEvents =
            one string "includePastEvents" queryString == Just "True"

        model =
            initialModel
                |> initializeIncludedAudiences audiences
                |> initializeIncludedLanguages languages
                |> initializeIncludedLocations locations
                |> initializeIncludedTopics topics
                |> update (IncludePastEvents includePastEvents)
                |> Tuple.first
    in
        ( model, Cmd.none )



-- Update


type Msg
    = NoOp
    | UpdateAudience (FilteredTagSection.Msg Audience)
    | UpdateLanguage (FilteredTagSection.Msg Language)
    | UpdateLocation (FilteredTagSection.Msg Location)
    | UpdateTopic (FilteredTagSection.Msg Topic)
    | IncludePastEvents Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UpdateAudience action ->
            let
                newModel =
                    { model | audiences = FilteredTagSection.update action model.audiences }
            in
                ( newModel, updateQueryString newModel )

        UpdateLanguage action ->
            let
                newModel =
                    { model | languages = FilteredTagSection.update action model.languages }
            in
                ( newModel, updateQueryString newModel )

        UpdateLocation action ->
            let
                newModel =
                    { model | locations = FilteredTagSection.update action model.locations }
            in
                ( newModel, updateQueryString newModel )

        UpdateTopic action ->
            let
                newModel =
                    { model | topics = FilteredTagSection.update action model.topics }
            in
                ( newModel, updateQueryString newModel )

        IncludePastEvents shouldIncludePastEvents ->
            let
                newModel =
                    { model | includePastEvents = shouldIncludePastEvents }
            in
                ( newModel, updateQueryString newModel )


urlUpdate : QueryString -> Model -> ( Model, Cmd Msg )
urlUpdate =
    (\_ model -> ( model, Cmd.none ))


updateQueryString : Model -> Cmd Msg
updateQueryString model =
    generateQueryString model
        |> modifyUrl


generateQueryString : Model -> String
generateQueryString model =
    List.foldr (getAudienceName >> add "audience") empty (includedAudiences model)
        |> (\queryString -> List.foldr (getLanguageName >> add "language") queryString (includedLanguages model))
        |> (\queryString -> List.foldr (getLocationName >> add "location") queryString (includedLocations model))
        |> (\queryString -> List.foldr (getTopicName >> add "topic") queryString (includedTopics model))
        |> (if model.includePastEvents then
                add "includePastEvents" (toString model.includePastEvents)
            else
                identity
           )
        |> render


initializeIncludedAudiences : List Audience -> Model -> Model
initializeIncludedAudiences includedAudiences model =
    { model | audiences = FilteredTagSection.initializeIncludedTags includedAudiences model.audiences }


initializeIncludedLanguages : List Language -> Model -> Model
initializeIncludedLanguages includedLanguages model =
    { model | languages = FilteredTagSection.initializeIncludedTags includedLanguages model.languages }


initializeIncludedLocations : List Location -> Model -> Model
initializeIncludedLocations includedLocations model =
    { model | locations = FilteredTagSection.initializeIncludedTags includedLocations model.locations }


initializeIncludedTopics : List Topic -> Model -> Model
initializeIncludedTopics includedTopics model =
    { model | topics = FilteredTagSection.initializeIncludedTags includedTopics model.topics }



-- Public functions


includedAudiences : Model -> List Audience
includedAudiences model =
    FilteredTagSection.includedTags model.audiences


includedLanguages : Model -> List Language
includedLanguages model =
    FilteredTagSection.includedTags model.languages


includedLocations : Model -> List Location
includedLocations model =
    FilteredTagSection.includedTags model.locations


includedTopics : Model -> List Topic
includedTopics model =
    FilteredTagSection.includedTags model.topics



-- View


view : Model -> Html.Html Msg
view model =
    Html.div [ class "container" ] <|
        List.concat
            [ languagesView model.languages
            , audiencesView model.audiences
            , topicsView model.topics
            , locationsView model.locations
            , [ includePastEventsButtonView model.includePastEvents ]
              --fix this
            , [ Html.map UpdateTopic FilteredTagSection.resetButtonView ]
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
    List.map Conference.view model.conferences


audiencesView : FilteredTagSection Audience -> List (Html.Html Msg)
audiencesView audiences =
    FilteredTagSection.view getAudienceName audiences
        |> List.map (Html.map UpdateAudience)


languagesView : FilteredTagSection Language -> List (Html.Html Msg)
languagesView languages =
    FilteredTagSection.view getLanguageName languages
        |> List.map (Html.map UpdateLanguage)


locationsView : FilteredTagSection Location -> List (Html.Html Msg)
locationsView locations =
    FilteredTagSection.view getLocationName locations
        |> List.map (Html.map UpdateLocation)


topicsView : FilteredTagSection Topic -> List (Html.Html Msg)
topicsView topics =
    FilteredTagSection.view getTopicName topics
        |> List.map (Html.map UpdateTopic)
