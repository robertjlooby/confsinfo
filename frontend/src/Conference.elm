module Conference exposing (Conference, decoder, view, CFPStatus(..))

import DateFormatter exposing (formatDay)
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Json.Decode as Decode exposing (Decoder, fail, field, string, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Time.Date as Date exposing (Date, fromISO8601)


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


decoder : Decoder Conference
decoder =
    decode Conference
        |> required "name" string
        |> required "link" string
        |> required "startDate" dateDecoder
        |> required "endDate" dateDecoder
        |> required "location" string
        |> required "cfpStatus" cfpDecoder


dateDecoder : Decoder Date
dateDecoder =
    Decode.map fromISO8601 string
        |> Decode.andThen
            (\result ->
                case result of
                    Ok date ->
                        succeed date

                    Err msg ->
                        fail msg
            )


cfpDecoder : Decoder CFPStatus
cfpDecoder =
    field "status" string
        |> Decode.andThen
            (\status ->
                case status of
                    "Open" ->
                        field "date" dateDecoder
                            |> Decode.map Open

                    "NotYetOpen" ->
                        field "date" dateDecoder
                            |> Decode.map NotYetOpen

                    "Closed" ->
                        succeed Closed

                    _ ->
                        fail <| "Invalid status: " ++ status
            )



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
