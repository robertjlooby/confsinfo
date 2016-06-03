module FilteredTagSection exposing (Model, initializeIncludedTags, includedTags, Msg, update, view, resetButtonView)

import FilteredTag
import FilteredTagSectionInternal exposing (..)
import Html exposing (text)
import Html.App as App
import Html.Attributes exposing (class)
import Html.Events
import Tag exposing (Tag)


-- Model


type alias Model =
    { sectionName : String
    , tags : List FilteredTag.Model
    }


includedTags : Model -> List Tag
includedTags model =
    List.filter FilteredTag.isIncluded model.tags
        |> List.map .tag



-- Update


type alias Msg =
    FilteredTagSectionInternal.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTag tag tagAction ->
            let
                updateTag filteredTag =
                    if filteredTag.tag == tag then
                        FilteredTag.update tagAction filteredTag
                    else
                        filteredTag
            in
                { model | tags = List.map updateTag model.tags }

        Reset ->
            { model | tags = List.map FilteredTag.exclude model.tags }


initializeIncludedTags : List String -> Model -> Model
initializeIncludedTags includedTags model =
    { model | tags = List.map (FilteredTag.initializeIncludedTag includedTags) model.tags }



-- View


view : Model -> List (Html.Html Msg)
view { sectionName, tags } =
    [ Html.div [ class "row" ]
        [ Html.h5 [] [ text sectionName ] ]
    , Html.div [ class "row" ]
        <| List.map (\tag -> App.map (UpdateTag tag.tag) (FilteredTag.view tag)) tags
    ]


resetButtonView : Html.Html Msg
resetButtonView =
    Html.div [ class "row" ]
        [ Html.button
            [ class "two columns offset-by-five"
            , Html.Events.onClick Reset
            ]
            [ text "Reset" ]
        ]
