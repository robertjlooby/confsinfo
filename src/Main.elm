import Conferences exposing (Conference, Model, Tag)
import DateFormatter
import Html exposing (text)
import Html.Attributes exposing (href)
import StartApp.Simple exposing (start)

view : Signal.Address Conferences.Action -> Model -> Html.Html
view address model =
  Html.div []
  [ Html.header [] [text "Confs.io"]
  , Html.table [] <| conferencesView address model
  ]

conferencesView : Signal.Address Conferences.Action -> Model -> List Html.Html
conferencesView address model =
  List.map (conferenceView address) model.conferences


conferenceView : Signal.Address Conferences.Action -> Conference -> Html.Html
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
