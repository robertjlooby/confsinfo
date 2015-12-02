import Html exposing (text)
import Date

type alias Model =
  { conferences : List Conference
  }

type alias Conference =
  { name : String
  , startDate : Date.Date
  , endDate : Date.Date
  }

view : Model -> Html.Html
view model =
  text "Hello, World"


main : Html.Html
main =
  view initialState

initialState : Model
initialState =
  { conferences = []
  }
