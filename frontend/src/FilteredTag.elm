module FilteredTag exposing (init, FilteredTag, Msg(..), decoder, update, view, initializeIncludedTag, exclude)

import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, required)
import Tag exposing (..)


-- Model


type alias FilteredTag tag =
    { tag : tag
    , included : Bool
    }


init : tag -> FilteredTag tag
init tag =
    { tag = tag
    , included = False
    }


decoder : (String -> tag) -> Decoder (FilteredTag tag)
decoder makeTag =
    Decode.map (\name -> FilteredTag (makeTag name) False) string


type Msg
    = Include
    | Exclude


update : Msg -> FilteredTag tag -> FilteredTag tag
update msg model =
    case msg of
        Include ->
            { model | included = True }

        Exclude ->
            { model | included = False }



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



-- View


view : (tag -> String) -> FilteredTag tag -> Html.Html Msg
view getTagName filteredTag =
    let
        ( tagString, tagClass, clickAction ) =
            case ( filteredTag.included, filteredTag.tag ) of
                ( True, tag ) ->
                    ( "- " ++ getTagName tag, "included", Exclude )

                ( False, tag ) ->
                    ( "+ " ++ getTagName tag, "excluded", Include )
    in
        Html.button
            [ class tagClass
            , Html.Events.onClick clickAction
            ]
            [ text tagString ]
