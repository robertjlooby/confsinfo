module Main (..) where

import Conference exposing (Conference)
import Model exposing (..)
import DateFormatter
import Debug
import FilteredTag exposing (FilteredTag(..))
import FilteredTagWithName exposing (FilteredTagWithName)
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Html.Events
import Task exposing (Task, andThen)
import TaskTutorial exposing (getCurrentTime)


view : Signal.Address Action -> Model -> Html.Html
view address model =
    Html.div
        [ class "container" ]
        <| List.concat
            [ allTagsView address model.tags
            , [ includePastEventsButtonView address model.includePastEvents ]
            , [ resetButtonView address ]
            , conferencesView address model
            , [ sourceCodeLink ]
            ]


includePastEventsButtonView : Signal.Address Action -> Bool -> Html.Html
includePastEventsButtonView address includePastEvents =
    let
        label = "Include Past Events"

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


allTagsView : Signal.Address Action -> List ( String, List FilteredTagWithName ) -> List Html.Html
allTagsView address tagsWithDescriptions =
    List.map (tagsView address) tagsWithDescriptions
        |> List.concat


tagsView : Signal.Address Action -> ( String, List FilteredTagWithName ) -> List Html.Html
tagsView address ( description, tags ) =
    [ Html.div
        [ class "row" ]
        [ Html.h5 [] [ text description ] ]
    , Html.div
        [ class "row" ]
        <| List.map (tagView address) tags
    ]


tagView : Signal.Address Action -> FilteredTagWithName -> Html.Html
tagView address tagWithDisplay =
    let
        ( tagString, tagClass, clickAction ) =
            case tagWithDisplay of
                ( Included t, d ) ->
                    ( "- " ++ d, "included", Exclude t )

                ( Excluded t, d ) ->
                    ( "+ " ++ d, "excluded", Include t )
    in
        Html.button
            [ class tagClass
            , Html.Events.onClick address clickAction
            ]
            [ text tagString ]


conferencesView : Signal.Address Action -> Model -> List Html.Html
conferencesView address model =
    let
        tagsToShow = List.map (\( _, tagsWithDisplay ) -> List.map fst tagsWithDisplay) model.tags |> List.concat
    in
        List.map (conferenceView address) (Model.shouldShow tagsToShow model.currentDate model.includePastEvents model.conferences)


conferenceView : Signal.Address Action -> Conference -> Html.Html
conferenceView address conference =
    Html.div
        [ class "row" ]
        [ Html.div
            [ class "five columns" ]
            [ Html.a [ href conference.link ] [ text conference.name ] ]
        , Html.div
            [ class "three columns" ]
            [ text <| DateFormatter.formatRange conference.startDate conference.endDate ]
        , Html.div
            [ class "four columns" ]
            [ text conference.location ]
        ]


actions : Signal.Mailbox (Maybe Model.Action)
actions =
    Signal.mailbox Nothing


address : Signal.Address Model.Action
address =
    Signal.forwardTo actions.address Just


update : Maybe Model.Action -> Model.Model -> Model.Model
update maybeAction model =
    case maybeAction of
        Just action ->
            Model.update action model

        Nothing ->
            Debug.crash "This should never happen."


initialModel : Model.Model
initialModel =
    let
        model = Model.initialState
    in
        case getStorage of
            Just tags ->
                Model.initialize tags model

            Nothing ->
                model


model : Signal Model.Model
model =
    Signal.foldp update initialModel actions.signal


main : Signal Html.Html
main =
    Signal.map (view address) model


port getStorage : Maybe (List String)
port setStorage : Signal (List String)
port setStorage =
    Signal.map Model.includedTags model


port title : String
port title =
    "confs.info"


port runner : Task x ()
port runner =
    getCurrentTime `andThen` (\time -> Signal.send actions.address (Just <| SetCurrentDate time))
