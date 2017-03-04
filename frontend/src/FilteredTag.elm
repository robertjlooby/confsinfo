module FilteredTag exposing (init, FilteredTag, Msg(..), State(..), update, view, initializeIncludedTag, isIncluded, exclude)

import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events
import Tag exposing (..)


-- Model


type State
    = Included
    | Excluded


type alias FilteredTag =
    { tag : Tag
    , state : State
    }


init : Tag -> FilteredTag
init tag =
    { tag = tag
    , state = Excluded
    }



-- Update


type Msg
    = Include
    | Exclude


update : Msg -> FilteredTag -> FilteredTag
update msg model =
    case msg of
        Include ->
            { model | state = Included }

        Exclude ->
            { model | state = Excluded }



-- Public functions


initializeIncludedTag : List String -> FilteredTag -> FilteredTag
initializeIncludedTag includedTags model =
    case model.tag of
        Tag.Tag tag ->
            if List.member tag includedTags then
                update Include model
            else
                model


exclude : FilteredTag -> FilteredTag
exclude model =
    update Exclude model


isIncluded : FilteredTag -> Bool
isIncluded model =
    case model.state of
        Included ->
            True

        Excluded ->
            False



-- View


view : FilteredTag -> Html.Html Msg
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
