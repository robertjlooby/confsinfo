module Main (main) where

import Debug
import Effects exposing (Effects)
import Html exposing (Html)
import InitialData
import Model
import StartApp exposing (start)
import Task exposing (Task, andThen)
import TaskTutorial exposing (getCurrentTime)


initialModel : Model.Model
initialModel =
  case getStorage of
    Just tags ->
      Model.initializeIncludedTags tags InitialData.model

    Nothing ->
      InitialData.model


app =
  StartApp.start
    { init = ( initialModel, initializeDate )
    , view = Model.view
    , update = Model.update
    , inputs = []
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


port getStorage : Maybe (List String)
port setStorage : Signal (List String)
port setStorage =
  Signal.map (Model.includedTags >> List.map toString) app.model


port title : String
port title =
  "confs.info"


initializeDate : Effects Model.Action
initializeDate =
  getCurrentTime
    |> Task.toMaybe
    |> Task.map Model.setCurrentDate
    |> Effects.task
