import Conferences exposing (..)
import DateFormatter
import Html exposing (text)
import Html.Attributes exposing (href)
import StartApp.Simple exposing (start)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
  [ Html.header [] [text "Confs.io"]
  , Html.ul [] <| tagsView address model.tags
  , Html.table [] <| conferencesView address model.conferences
  ]

tagsView : Signal.Address Action -> List FilteredTag -> List Html.Html
tagsView address tags =
  List.map (tagView address) tags

tagView : Signal.Address Action -> FilteredTag -> Html.Html
tagView address tag =
  let
    tagString = case tag of
                  Included t -> toString t
                  Excluded t -> toString t
    tagClass = case tag of
                  Included _ -> "included"
                  Excluded _ -> "excluded"
  in
    Html.li
      [Html.Attributes.class tagClass]
      [text tagString]

conferencesView : Signal.Address Action -> List Conference -> List Html.Html
conferencesView address conferences =
  List.map (conferenceView address) conferences


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
