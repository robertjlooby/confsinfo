module FilteredTagSection exposing (FilteredTagSection, initializeIncludedTags, includedTags, excludeAll, Msg(..), update, view)

import FilteredTag exposing (FilteredTag)
import Html exposing (text)
import Html.Attributes exposing (class)
import Html.Events


-- Model


type alias FilteredTagSection tag =
    { sectionName : String
    , tags : List (FilteredTag tag)
    }


includedTags : FilteredTagSection tag -> List tag
includedTags model =
    List.filter FilteredTag.isIncluded model.tags
        |> List.map .tag



-- Update


type Msg tag
    = UpdateTag tag FilteredTag.Msg


update : Msg tag -> FilteredTagSection tag -> FilteredTagSection tag
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


initializeIncludedTags : FilteredTagSection tag -> List tag -> FilteredTagSection tag
initializeIncludedTags filteredTagSection includedTags =
    { filteredTagSection
        | tags = List.map (FilteredTag.initializeIncludedTag includedTags) filteredTagSection.tags
    }


excludeAll : FilteredTagSection tag -> FilteredTagSection tag
excludeAll model =
    { model | tags = List.map FilteredTag.exclude model.tags }



-- View


view : (tag -> String) -> FilteredTagSection tag -> List (Html.Html (Msg tag))
view getTagName { sectionName, tags } =
    [ Html.div [ class "row" ]
        [ Html.h5 [] [ text sectionName ] ]
    , Html.div [ class "row" ] <|
        List.map (\tag -> Html.map (UpdateTag tag.tag) (FilteredTag.view getTagName tag)) tags
    ]
