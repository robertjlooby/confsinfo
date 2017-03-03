module Conference exposing (Model, CFPStatus(..), cfpStatus, shouldShow, view)

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



-- Public functions


shouldShow : List Tag -> Model -> Bool
shouldShow includedTags conference =
    List.all (\tag -> List.member tag conference.tags) includedTags



-- View


type CFPStatus
    = Closed
    | NotYetOpen
    | Open


cfpStatus : Date -> Model -> ( CFPStatus, Maybe Date )
cfpStatus currentDate conference =
    case Maybe.map (Date.compare currentDate) conference.cfpEndDate of
        Nothing ->
            ( Closed, Nothing )

        Just GT ->
            ( Closed, Nothing )

        Just _ ->
            case Maybe.map (Date.compare currentDate) conference.cfpStartDate of
                Just LT ->
                    ( NotYetOpen, conference.cfpStartDate )

                _ ->
                    ( Open, conference.cfpEndDate )


view : Date -> Model -> Html.Html msg
view currentDate conference =
    Html.div [ class "row" ]
        [ conferenceNameHtml conference currentDate
        , Html.div [ class "three columns" ]
            [ text <| DateFormatter.formatRange conference.startDate conference.endDate ]
        , Html.div [ class "four columns" ]
            [ text conference.location ]
        ]


conferenceNameHtml : Model -> Date -> Html.Html msg
conferenceNameHtml conference currentDate =
    let
        nameLink =
            Html.a [ href conference.link ] [ text conference.name ]

        inner =
            case cfpStatus currentDate conference of
                ( Open, Just endDate ) ->
                    [ nameLink
                    , Html.small
                        [ class "cfp cfp-open"
                        , Html.Attributes.title <| "Closes " ++ DateFormatter.formatDate endDate
                        ]
                        [ text "CFP open" ]
                    ]

                ( NotYetOpen, Just startDate ) ->
                    [ nameLink
                    , Html.small [ class "cfp" ]
                        [ text <| "CFP opens " ++ DateFormatter.formatDate startDate ]
                    ]

                _ ->
                    [ nameLink ]
    in
        Html.div [ class "five columns" ]
            inner
