module Main exposing (main)

import Html exposing (Html)
import Html.App as App
import InitialData
import Model
import Task exposing (Task, andThen)
import Time


init : Maybe (List String) -> ( Model.Model, Cmd Model.Msg )
init maybeTags =
    let
        model =
            case maybeTags of
                Just tags ->
                    Model.initializeIncludedTags tags InitialData.model

                Nothing ->
                    InitialData.model
    in
        ( model, initializeDate )


main : Program (Maybe (List String))
main =
    App.programWithFlags
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
