import Conferences exposing (..)
import DateFormatter
import Debug
import Html exposing (text)
import Html.Attributes exposing (href)
import Html.Events

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
  [ allTagsView address model.tags
  , Html.button [ Html.Events.onClick address Reset ] [ text "Reset" ]
  , Html.table [] <| conferencesView address model.conferences model.tags
  ]

allTagsView : Signal.Address Action -> List (String, List (FilteredTag, String)) -> Html.Html
allTagsView address tagsWithDescriptions =
  Html.div [] <| List.map (tagsView address) tagsWithDescriptions

tagsView : Signal.Address Action -> (String, List (FilteredTag, String)) -> Html.Html
tagsView address (description, tags) =
  Html.div
    []
    [ Html.h4 [] [ text description ]
    , Html.ul [Html.Attributes.class "tags-list"] <| List.map (tagView address) tags
    ]

tagView : Signal.Address Action -> (FilteredTag, String) -> Html.Html
tagView address tagWithDisplay =
  let
    (tagString, tagClass, clickAction) = case tagWithDisplay of
                  (Included t, d) -> ("- " ++ d, "included", Exclude t)
                  (Excluded t, d) -> ("+ " ++ d, "excluded", Include t)
  in
    Html.li
      [ Html.Attributes.class tagClass
      , Html.Events.onClick address clickAction
      ]
      [ Html.button [] [ text tagString ]]

conferencesView : Signal.Address Action -> List Conference -> List (String, List (FilteredTag, String)) -> List Html.Html
conferencesView address conferences tagsWithDescriptions =
  let
    tagsToShow = List.map (\(_, tagsWithDisplay) -> List.map fst tagsWithDisplay) tagsWithDescriptions |> List.concat
  in
    List.map (conferenceView address) (Conferences.shouldShow tagsToShow conferences)


conferenceView : Signal.Address Action -> Conference -> Html.Html
conferenceView address conference =
  Html.tr
    []
    [ Html.td
        []
        [Html.a [href conference.link] [text conference.name]]
    , Html.td
        []
        [text <| DateFormatter.formatRange conference.startDate conference.endDate]
    , Html.td
        []
        [text conference.location]
    ]

actions : Signal.Mailbox (Maybe Conferences.Action)
actions = Signal.mailbox Nothing

address : Signal.Address Conferences.Action
address = Signal.forwardTo actions.address Just

update : (Maybe Conferences.Action) -> Conferences.Model -> Conferences.Model
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
port title = "confs.info"
