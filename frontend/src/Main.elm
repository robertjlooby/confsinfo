module Main exposing (main)

import Model
import InitialData
import Navigation
import Route.QueryString exposing (parse)


main : Program Never
main =
    Navigation.program (Navigation.makeParser (\{ search } -> parse search))
        { init = Model.init InitialData.model
        , view = Model.view
        , update = Model.update
        , urlUpdate = Model.urlUpdate
        , subscriptions = (\_ -> Sub.none)
        }
