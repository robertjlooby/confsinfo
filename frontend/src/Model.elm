module Model exposing (Model, Msg(..), init, update, urlUpdate, initializeIncludedTags, includedTags, view)

import Conference exposing (Conference)
import FilteredTagSection exposing (FilteredTagSection)
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Html.Events
import Navigation exposing (modifyUrl)
import QueryString exposing (QueryString, add, all, empty, one, render, string, parse)
import Tag exposing (Tag)


-- Model


type alias Model =
    { conferences : List Conference
    , includePastEvents : Bool
    , tags : List FilteredTagSection
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
        ( model, Cmd.none )



-- Update


type Msg
    = NoOp
    | UpdateTag FilteredTagSection.Msg
    | IncludePastEvents Bool


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


conferencesView : Model -> List (Html.Html Msg)
conferencesView model =
    List.map Conference.view model.conferences


allTagsView : List FilteredTagSection -> List (Html.Html Msg)
allTagsView filteredTagSections =
    List.map FilteredTagSection.view filteredTagSections
        |> List.concat
        |> List.map (Html.map UpdateTag)
