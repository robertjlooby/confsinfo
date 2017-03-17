module Model exposing (Model, Msg(..), decoder, generateQueryString, update, includedTopics, includedLanguages, includedLocations, includedAudiences, view)

import Conference exposing (Conference)
import FilteredTagSection exposing (FilteredTagSection)
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Html.Events
import Http
import Json.Decode as Decode exposing (Decoder, bool, list)
import Json.Decode.Pipeline exposing (decode, required)
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


decoder : Decoder Model
decoder =
    decode Model
        |> required "conferences" (list Conference.decoder)
        |> required "includePastEvents" bool
        |> required "audiences" (FilteredTagSection.decoder "Audiences" Audience)
        |> required "languages" (FilteredTagSection.decoder "Languages" Language)
        |> required "locations" (FilteredTagSection.decoder "Locations" Location)
        |> required "topics" (FilteredTagSection.decoder "Topics" Topic)



-- Update


type Msg
    = UpdateAudience (FilteredTagSection.Msg Audience)
    | UpdateLanguage (FilteredTagSection.Msg Language)
    | UpdateLocation (FilteredTagSection.Msg Location)
    | UpdateTopic (FilteredTagSection.Msg Topic)
    | Reset
    | IncludePastEvents Bool
    | SearchResult (Result Http.Error (List Conference))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        Reset ->
            let
                newModel =
                    { model
                        | audiences = FilteredTagSection.excludeAll model.audiences
                        , languages = FilteredTagSection.excludeAll model.languages
                        , locations = FilteredTagSection.excludeAll model.locations
                        , topics = FilteredTagSection.excludeAll model.topics
                    }
            in
                ( newModel, updateQueryString newModel )

        SearchResult (Ok conferences) ->
            { model | conferences = conferences } ! []

        SearchResult (Err message) ->
            let
                _ =
                    Debug.log "Search error:" message
            in
                model ! []


search : Model -> List (Cmd Msg)
search model =
    let
        query =
            generateQueryString model
    in
        [ modifyUrl query
        , Http.get ("/search" ++ query) (list Conference.decoder)
            |> Http.send SearchResult
        ]


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
            [ tagView UpdateLanguage getLanguageName model.languages
            , tagView UpdateAudience getAudienceName model.audiences
            , tagView UpdateTopic getTopicName model.topics
            , tagView UpdateLocation getLocationName model.locations
            , [ includePastEventsButtonView model.includePastEvents ]
            , [ resetButtonView ]
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


tagView : (FilteredTagSection.Msg tag -> Msg) -> (tag -> String) -> FilteredTagSection tag -> List (Html.Html Msg)
tagView msg show tags =
    FilteredTagSection.view show tags
        |> List.map (Html.map msg)


resetButtonView : Html.Html Msg
resetButtonView =
    Html.div [ class "row" ]
        [ Html.button
            [ class "two columns offset-by-five"
            , Html.Events.onClick Reset
            ]
            [ text "Reset" ]
        ]
