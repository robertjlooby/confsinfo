module Model exposing (Model, Msg(..), generateQueryString, init, update, urlUpdate, initializeIncludedTags, includedTags, includedLanguages, view)

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
    , languages : FilteredTagSection Language
    , tags : List (FilteredTagSection Tag)
    }


init : Model -> { navLocation | search : String } -> ( Model, Cmd Msg )
init initialModel { search } =
    let
        queryString =
            parse search

        languages =
            all "language" queryString
                |> List.map Language

        tags =
            all "tag" queryString
                |> List.map Tag

        includePastEvents =
            one string "includePastEvents" queryString == Just "True"

        model =
            initializeIncludedTags tags initialModel
                |> initializeIncludedLanguages languages
                |> update (IncludePastEvents includePastEvents)
                |> Tuple.first
    in
        ( model, Cmd.none )



-- Update


type Msg
    = NoOp
    | UpdateLanguage (FilteredTagSection.Msg Language)
    | UpdateTag (FilteredTagSection.Msg Tag)
    | IncludePastEvents Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UpdateLanguage action ->
            let
                newModel =
                    { model | languages = FilteredTagSection.update action model.languages }
            in
                ( newModel, updateQueryString newModel )

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


urlUpdate : QueryString -> Model -> ( Model, Cmd Msg )
urlUpdate =
    (\_ model -> ( model, Cmd.none ))


updateQueryString : Model -> Cmd Msg
updateQueryString model =
    generateQueryString model
        |> modifyUrl


generateQueryString : Model -> String
generateQueryString model =
    List.foldr (getLanguageName >> add "language") empty (includedLanguages model)
        |> (\queryString -> List.foldr (getTagName >> add "tag") queryString (includedTags model))
        |> (if model.includePastEvents then
                add "includePastEvents" (toString model.includePastEvents)
            else
                identity
           )
        |> render


initializeIncludedLanguages : List Language -> Model -> Model
initializeIncludedLanguages includedLanguages model =
    { model | languages = FilteredTagSection.initializeIncludedTags includedLanguages model.languages }


initializeIncludedTags : List Tag -> Model -> Model
initializeIncludedTags includedTags model =
    { model | tags = List.map (FilteredTagSection.initializeIncludedTags includedTags) model.tags }



-- Public functions


includedLanguages : Model -> List Language
includedLanguages model =
    FilteredTagSection.includedTags model.languages


includedTags : Model -> List Tag
includedTags model =
    List.map FilteredTagSection.includedTags model.tags
        |> List.concat



-- View


view : Model -> Html.Html Msg
view model =
    Html.div [ class "container" ] <|
        List.concat
            [ languagesView model.languages
            , allTagsView model.tags
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


conferencesView : Model -> List (Html.Html Msg)
conferencesView model =
    List.map Conference.view model.conferences


languagesView : FilteredTagSection Language -> List (Html.Html Msg)
languagesView languages =
    FilteredTagSection.view getLanguageName languages
        |> List.map (Html.map UpdateLanguage)


allTagsView : List (FilteredTagSection Tag) -> List (Html.Html Msg)
allTagsView filteredTagSections =
    List.map (FilteredTagSection.view getTagName) filteredTagSections
        |> List.concat
        |> List.map (Html.map UpdateTag)
