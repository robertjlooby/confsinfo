module WebModel exposing (Msg(..), WebModel, init, update, view)

import Html exposing (Html, div, hr, section, text)
import Html.Attributes exposing (class)
import Http
import Model exposing (Model)
import RemoteData exposing (WebData)


type alias WebModel =
    { model : WebData Model }


init : { navLocation | search : String } -> ( WebModel, Cmd Msg )
init { search } =
    { model = RemoteData.Loading } ! [ initialSearch search ]


initialSearch : String -> Cmd Msg
initialSearch query =
    Http.get ("/init" ++ query) Model.decoder
        |> RemoteData.sendRequest
        |> Cmd.map Initialize


type Msg
    = NoOp
    | Initialize (WebData Model)
    | ModelMsg Model.Msg


update : Msg -> WebModel -> ( WebModel, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Initialize newModel ->
            { model | model = newModel } ! []

        ModelMsg modelMsg ->
            let
                ( newModel, newModelMsg ) =
                    RemoteData.update (Model.update modelMsg) model.model
            in
                { model | model = newModel } ! [ Cmd.map ModelMsg newModelMsg ]


view : WebModel -> Html Msg
view model =
    case model.model of
        RemoteData.Success model ->
            Model.view model
                |> Html.map ModelMsg

        _ ->
            div [ class "container" ]
                [ hr [] []
                , section [ class "header" ]
                    [ div [ class "row" ]
                        [ div
                            [ class "two columns offset-by-five" ]
                            [ text "Loading..." ]
                        ]
                    ]
                ]
