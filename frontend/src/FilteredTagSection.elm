module FilteredTagSection exposing (FilteredTagSection, initializeIncludedTags, includedTags, Msg(..), update, view, resetButtonView)

import FilteredTag exposing (FilteredTag)
import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events
import Tag exposing (Tag)


-- Model


type alias FilteredTagSection =
    { sectionName : String
    , tags : List FilteredTag
    }


includedTags : FilteredTagSection -> List Tag
includedTags model =
    List.filter FilteredTag.isIncluded model.tags
        |> List.map .tag



-- Update


type Msg
    = UpdateTag Tag FilteredTag.Msg
    | Reset


update : Msg -> FilteredTagSection -> FilteredTagSection
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


initializeIncludedTags : List String -> FilteredTagSection -> FilteredTagSection
initializeIncludedTags includedTags model =
    { model | tags = List.map (FilteredTag.initializeIncludedTag includedTags) model.tags }



-- View


view : FilteredTagSection -> List (Html.Html Msg)
view { sectionName, tags } =
    [ Html.div [ class "row" ]
        [ Html.h5 [] [ text sectionName ] ]
    , Html.div [ class "row" ] <|
        List.map (\tag -> Html.map (UpdateTag tag.tag) (FilteredTag.view tag)) tags
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
