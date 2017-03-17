module Main exposing (main)

import WebModel exposing (Msg, WebModel)
import Navigation


main : Program Never WebModel Msg
main =
    Navigation.program (\_ -> WebModel.NoOp)
        { init = WebModel.init
        , view = WebModel.view
        , update = WebModel.update
        , subscriptions = (\_ -> Sub.none)
        }
