module FilteredTag exposing (init, FilteredTag, Msg(..), State(..), update, view, initializeIncludedTag, isIncluded, exclude)

import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events
import Tag exposing (..)


-- Model


type State
    = Included
    | Excluded


type alias FilteredTag tag =
    { tag : tag
    , state : State
    }


init : tag -> FilteredTag tag
init tag =
    { tag = tag
    , state = Excluded
    }



-- Update


type Msg
    = Include
    | Exclude


update : Msg -> FilteredTag tag -> FilteredTag tag
update msg model =
    case msg of
        Include ->
            { model | state = Included }

        Exclude ->
            { model | state = Excluded }



-- Public functions


initializeIncludedTag : List tag -> FilteredTag tag -> FilteredTag tag
initializeIncludedTag includedTags filteredTag =
    if List.member filteredTag.tag includedTags then
        update Include filteredTag
    else
        filteredTag


exclude : FilteredTag tag -> FilteredTag tag
exclude model =
    update Exclude model


isIncluded : FilteredTag tag -> Bool
isIncluded model =
    case model.state of
        Included ->
            True

        Excluded ->
            False



-- View


view : (tag -> String) -> FilteredTag tag -> Html.Html Msg
view getTagName filteredTag =
    let
        ( tagString, tagClass, clickAction ) =
            case ( filteredTag.state, filteredTag.tag ) of
                ( Included, tag ) ->
                    ( "- " ++ getTagName tag, "included", Exclude )

                ( Excluded, tag ) ->
                    ( "+ " ++ getTagName tag, "excluded", Include )
    in
        Html.button
            [ class tagClass
            , Html.Events.onClick clickAction
            ]
            [ text tagString ]
