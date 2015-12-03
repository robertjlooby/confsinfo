import Conferences exposing (Conference, Model, Tag)
import DateFormatter
import Html exposing (text)
import Html.Attributes exposing (href)

view : Model -> Html.Html
view model =
  Html.div []
  [ Html.header [] [text "Confs.io"]
  , Html.table [] <| conferencesView model
  ]

conferencesView : Model -> List Html.Html
conferencesView model =
  List.map conferenceView model.conferences


conferenceView : Conference -> Html.Html
conferenceView conference =
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

main : Html.Html
main =
  view Conferences.list
