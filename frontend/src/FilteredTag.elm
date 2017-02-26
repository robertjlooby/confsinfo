module FilteredTag exposing (init, Model, Msg(..), State(..), update, view, initializeIncludedTag, isIncluded, exclude)

import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events
import Tag exposing (..)


-- Model


type State
    = Included
    | Excluded


type alias Model =
    { tag : Tag
    , state : State
    }


init : Tag -> Model
init tag =
    { tag = tag
    , state = Excluded
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
    case model.tag of
        Tag.Tag tag ->
            if List.member tag includedTags then
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
            case ( model.state, model.tag ) of
                ( Included, Tag tag ) ->
                    ( "- " ++ tag, "included", Exclude )

                ( Excluded, Tag tag ) ->
                    ( "+ " ++ tag, "excluded", Include )
    in
        Html.button
            [ class tagClass
            , Html.Events.onClick clickAction
            ]
            [ text tagString ]
