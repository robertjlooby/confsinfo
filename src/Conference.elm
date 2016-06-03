module Conference exposing (Model, shouldShow, compare', view)

import ConferenceInternal exposing (..)
import DaTuple exposing (DaTuple)
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Tag exposing (Tag)


-- Model


type alias Model =
  ConferenceInternal.Model



-- Public functions


shouldShow : List Tag -> Model -> Bool
shouldShow includedTags conference =
  List.all (\tag -> List.member tag conference.tags) includedTags


compare' : Model -> Model -> Order
compare' conf conf' =
  let
    dateCompare =
      DaTuple.compare' conf.startDate conf'.startDate
  in
    if dateCompare == EQ then
      compare conf.name conf'.name
    else
      dateCompare



-- View


type alias CFPStatus =
  ConferenceInternal.CFPStatus


cfpStatus : DaTuple -> Model -> ( CFPStatus, Maybe DaTuple )
cfpStatus =
  ConferenceInternal.cfpStatus


view : DaTuple -> Model -> Html.Html msg
view currentDate conference =
  Html.div
    [ class "row" ]
    [ conferenceNameHtml conference currentDate
    , Html.div
        [ class "three columns" ]
        [ text <| DaTuple.formatRange conference.startDate conference.endDate ]
    , Html.div
        [ class "four columns" ]
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
          , Html.small
              [ class "cfp" ]
              [ text <| "CFP opens " ++ DaTuple.formatDate startDate ]
          ]

        _ ->
          [ nameLink ]
  in
    Html.div
      [ class "five columns" ]
      inner
