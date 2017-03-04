module Conference exposing (Model, CFPStatus(..), cfpStatus, view)

import DateFormatter
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Tag exposing (Tag)
import Time.Date as Date exposing (Date)


-- Model


type alias Model =
    { name : String
    , link : String
    , startDate : Date
    , endDate : Date
    , location : String
    , cfpStartDate : Maybe Date
    , cfpEndDate : Maybe Date
    , tags : List Tag
    }



-- View


type CFPStatus
    = Closed
    | NotYetOpen
    | Open


cfpStatus : Model -> CFPStatus
cfpStatus conference =
    Closed


view : Model -> Html.Html msg
view conference =
    Html.div [ class "row" ]
        [ conferenceNameHtml conference
        , Html.div [ class "three columns" ]
            [ text <| DateFormatter.formatRange conference.startDate conference.endDate ]
        , Html.div [ class "four columns" ]
            [ text conference.location ]
        ]


conferenceNameHtml : Model -> Html.Html msg
conferenceNameHtml conference =
    let
        nameLink =
            Html.a [ href conference.link ] [ text conference.name ]

        inner =
            case cfpStatus conference of
                Open ->
                    [ nameLink
                    , Html.small
                        [ class "cfp cfp-open"
                        , Html.Attributes.title "Closes some time"
                        ]
                        [ text "CFP open" ]
                    ]

                NotYetOpen ->
                    [ nameLink
                    , Html.small [ class "cfp" ]
                        [ text "CFP opens some time" ]
                    ]

                _ ->
                    [ nameLink ]
    in
        Html.div [ class "five columns" ]
            inner
