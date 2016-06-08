module Main exposing (main)

import Html exposing (Html)
import Html.App as App
import InitialData
import Model
import Task exposing (Task, andThen)
import Time


init : ( Model.Model, Cmd Model.Msg )
init =
    ( InitialData.model, initializeDate )


main : Program Never
main =
    App.program
        { init = init
        , view = Model.view
        , update = Model.update
        , subscriptions = (\_ -> Sub.none)
        }


initializeDate : Cmd Model.Msg
initializeDate =
    Task.perform (\_ -> Model.setCurrentDate Nothing)
        (\time -> Model.setCurrentDate <| Just time)
        Time.now
