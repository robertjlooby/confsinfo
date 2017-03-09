module Conference exposing (Conference, view, CFPStatus(..))

import DateFormatter exposing (formatDay)
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Time.Date as Date exposing (Date)


-- Model


type alias Conference =
    { name : String
    , link : String
    , startDate : Date
    , endDate : Date
    , location : String
    , cfpStatus : CFPStatus
    }


type CFPStatus
    = Closed
    | NotYetOpen Date
    | Open Date



-- View


view : Conference -> Html.Html msg
view conference =
    Html.div [ class "row" ]
        [ conferenceNameHtml conference
        , Html.div [ class "three columns" ]
            [ text <| DateFormatter.formatRange conference.startDate conference.endDate ]
        , Html.div [ class "four columns" ]
            [ text conference.location ]
        ]


conferenceNameHtml : Conference -> Html.Html msg
conferenceNameHtml conference =
    let
        nameLink =
            Html.a [ href conference.link ] [ text conference.name ]

        inner =
            case conference.cfpStatus of
                Open closeDate ->
                    [ nameLink
                    , Html.small
                        [ class "cfp cfp-open"
                        , Html.Attributes.title <| "Closes " ++ formatDay closeDate
                        ]
                        [ text "CFP open" ]
                    ]

                NotYetOpen openDate ->
                    [ nameLink
                    , Html.small [ class "cfp" ]
                        [ text <| "CFP opens " ++ formatDay openDate ]
                    ]

                _ ->
                    [ nameLink ]
    in
        Html.div [ class "five columns" ]
            inner
