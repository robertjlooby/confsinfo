module Conference exposing (Model, CFPStatus(..), cfpStatus, shouldShow, view)

import DaTuple exposing (DaTuple)
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Tag exposing (Tag)


-- Model


type alias Model =
    { name : String
    , link : String
    , startDate : DaTuple
    , endDate : DaTuple
    , location : String
    , cfpStartDate : Maybe DaTuple
    , cfpEndDate : Maybe DaTuple
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


cfpStatus : DaTuple -> Model -> ( CFPStatus, Maybe DaTuple )
cfpStatus currentDate conference =
    case Maybe.map (DaTuple.compareDaTuples currentDate) conference.cfpEndDate of
        Nothing ->
            ( Closed, Nothing )

        Just GT ->
            ( Closed, Nothing )

        Just _ ->
            case Maybe.map (DaTuple.compareDaTuples currentDate) conference.cfpStartDate of
                Just LT ->
                    ( NotYetOpen, conference.cfpStartDate )

                _ ->
                    ( Open, conference.cfpEndDate )


view : DaTuple -> Model -> Html.Html msg
view currentDate conference =
    Html.div [ class "row" ]
        [ conferenceNameHtml conference currentDate
        , Html.div [ class "three columns" ]
            [ text <| DaTuple.formatRange conference.startDate conference.endDate ]
        , Html.div [ class "four columns" ]
            [ text conference.location ]
        ]


conferenceNameHtml : Model -> DaTuple -> Html.Html msg
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
                        , Html.Attributes.title <| "Closes " ++ DaTuple.formatDate endDate
                        ]
                        [ text "CFP open" ]
                    ]

                ( NotYetOpen, Just startDate ) ->
                    [ nameLink
                    , Html.small [ class "cfp" ]
                        [ text <| "CFP opens " ++ DaTuple.formatDate startDate ]
                    ]

                _ ->
                    [ nameLink ]
    in
        Html.div [ class "five columns" ]
            inner
