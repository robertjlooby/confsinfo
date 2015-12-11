import Conferences exposing (..)
import DateFormatter
import Html exposing (text)
import Html.Attributes exposing (href)
import Html.Events
import StartApp.Simple exposing (start)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
  [ Html.header [] [text "confs.info"]
  , allTagsView address model.tags
  , Html.button [ Html.Events.onClick address Reset ] [ text "Reset" ]
  , Html.table [] <| conferencesView address model.conferences model.tags
  ]

allTagsView : Signal.Address Action -> List (String, List FilteredTag) -> Html.Html
allTagsView address tagsWithDescriptions =
  Html.div [] <| List.map (tagsView address) tagsWithDescriptions

tagsView : Signal.Address Action -> (String, List FilteredTag) -> Html.Html
tagsView address (description, tags) =
  Html.div
    []
    [ Html.h4 [] [ text description ]
    , Html.ul [Html.Attributes.class "tags-list"] <| List.map (tagView address) tags
    ]

tagView : Signal.Address Action -> FilteredTag -> Html.Html
tagView address tag =
  let
    (tagString, tagClass, clickAction) = case tag of
                  Included t -> ("- " ++ toString t, "included", Exclude t)
                  Excluded t -> ("+ " ++ toString t, "excluded", Include t)
  in
    Html.li
      [ Html.Attributes.class tagClass
      , Html.Events.onClick address clickAction
      ]
      [ Html.button [] [ text tagString ]]

conferencesView : Signal.Address Action -> List Conference -> List (String, List FilteredTag) -> List Html.Html
conferencesView address conferences tagsWithDescriptions =
  let
    tagsToShow = List.map (\(_, tags) -> tags) tagsWithDescriptions |> List.concat
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

main : Signal Html.Html
main =
  start
    { model = Conferences.list
    , update = Conferences.update
    , view = view
    }

port title : String
port title = "confs.info"
