module Main exposing (main)

import Model
import InitialData
import Navigation


main : Program Never Model.Model Model.Msg
main =
    Navigation.program (\_ -> Model.NoOp)
        { init = Model.init InitialData.model
        , view = Model.view
        , update = Model.update
        , subscriptions = (\_ -> Sub.none)
        }
