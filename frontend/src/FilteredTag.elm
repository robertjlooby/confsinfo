module FilteredTag exposing (init, Model, Msg(..), State(..), update, view, initializeIncludedTag, isIncluded, exclude)

import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events
import Tag exposing (Tag)


-- Model


type State
    = Included
    | Excluded


type alias Model =
    { tag : Tag
    , state : State
    , display : String
    }


init : Tag -> String -> Model
init tag display =
    { tag = tag
    , state = Excluded
    , display = display
    }



-- Update


type Msg
    = Include
    | Exclude


update : Msg -> Model -> Model
update msg model =
    case msg of
        Include ->
            { model | state = Included }

        Exclude ->
            { model | state = Excluded }



-- Public functions


initializeIncludedTag : List String -> Model -> Model
initializeIncludedTag includedTags model =
    if List.member (toString model.tag) includedTags then
        update Include model
    else
        model


exclude : Model -> Model
exclude model =
    update Exclude model


isIncluded : Model -> Bool
isIncluded model =
    case model.state of
        Included ->
            True

        Excluded ->
            False



-- View


view : Model -> Html.Html Msg
view model =
    let
        ( tagString, tagClass, clickAction ) =
            case model.state of
                Included ->
                    ( "- " ++ model.display, "included", Exclude )

                Excluded ->
                    ( "+ " ++ model.display, "excluded", Include )
    in
        Html.button
            [ class tagClass
            , Html.Events.onClick clickAction
            ]
            [ text tagString ]
