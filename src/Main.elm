module Main (main) where

import Debug
import Html
import InitialData
import Model
import Task exposing (Task, andThen)
import TaskTutorial exposing (getCurrentTime)


actions : Signal.Mailbox (Maybe Model.Action)
actions =
  Signal.mailbox Nothing


address : Signal.Address Model.Action
address =
  Signal.forwardTo actions.address Just


update : Maybe Model.Action -> Model.Model -> Model.Model
update maybeAction model =
  case maybeAction of
    Just action ->
      Model.update action model

    Nothing ->
      Debug.crash "This should never happen."


initialModel : Model.Model
initialModel =
  case getStorage of
    Just tags ->
      Model.initializeIncludedTags tags InitialData.model

    Nothing ->
      InitialData.model


model : Signal Model.Model
model =
  Signal.foldp update initialModel actions.signal


main : Signal Html.Html
main =
  Signal.map (Model.view address) model


port getStorage : Maybe (List String)
port setStorage : Signal (List String)
port setStorage =
  Signal.map (Model.includedTags >> List.map toString) model


port title : String
port title =
  "confs.info"


port runner : Task x ()
port runner =
  getCurrentTime `andThen` (Model.setCurrentDate address)
