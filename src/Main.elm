module Main (..) where

import Conferences exposing (..)
import DateFormatter
import Debug
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Html.Events


view : Signal.Address Action -> Model -> Html.Html
view address model =
    Html.div
        [ class "container" ]
        <| List.concat
            [ allTagsView address model.tags
            , [ resetButtonView address ]
            , conferencesView address model.conferences model.tags
            , [ sourceCodeLink ]
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


allTagsView : Signal.Address Action -> List ( String, List ( FilteredTag, String ) ) -> List Html.Html
allTagsView address tagsWithDescriptions =
    List.map (tagsView address) tagsWithDescriptions
        |> List.concat


tagsView : Signal.Address Action -> ( String, List ( FilteredTag, String ) ) -> List Html.Html
tagsView address ( description, tags ) =
    [ Html.div
        [ class "row" ]
        [ Html.h5 [] [ text description ] ]
    , Html.div
        [ class "row" ]
        <| List.map (tagView address) tags
    ]


tagView : Signal.Address Action -> ( FilteredTag, String ) -> Html.Html
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


conferencesView : Signal.Address Action -> List Conference -> List ( String, List ( FilteredTag, String ) ) -> List Html.Html
conferencesView address conferences tagsWithDescriptions =
    let
        tagsToShow = List.map (\( _, tagsWithDisplay ) -> List.map fst tagsWithDisplay) tagsWithDescriptions |> List.concat
    in
        List.map (conferenceView address) (Conferences.shouldShow tagsToShow conferences)


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


actions : Signal.Mailbox (Maybe Conferences.Action)
actions =
    Signal.mailbox Nothing


address : Signal.Address Conferences.Action
address =
    Signal.forwardTo actions.address Just


update : Maybe Conferences.Action -> Conferences.Model -> Conferences.Model
update maybeAction model =
    case maybeAction of
        Just action ->
            Conferences.update action model

        Nothing ->
            Debug.crash "This should never happen."


initialModel : Conferences.Model
initialModel =
    let
        model = Conferences.list
    in
        case getStorage of
            Just tags ->
                Conferences.initialize tags model

            Nothing ->
                model


model : Signal Conferences.Model
model =
    Signal.foldp update initialModel actions.signal


main : Signal Html.Html
main =
    Signal.map (view address) model


port getStorage : Maybe (List String)
port setStorage : Signal (List String)
port setStorage =
    Signal.map Conferences.includedTags model


port title : String
port title =
    "confs.info"
