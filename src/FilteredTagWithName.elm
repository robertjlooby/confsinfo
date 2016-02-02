module FilteredTagWithName (..) where

import FilteredTag exposing (FilteredTag(..))
import Tag exposing (Tag)


type alias FilteredTagWithName =
  ( FilteredTag, String )


initializeFilteredTag : List String -> FilteredTagWithName -> FilteredTagWithName
initializeFilteredTag includedTags ( filteredTag, description ) =
  let
    tag =
      FilteredTag.getTag filteredTag
  in
    if List.member (toString tag) includedTags then
      ( Included tag, description )
    else
      ( filteredTag, description )


excludeTag : Tag -> FilteredTagWithName -> FilteredTagWithName
excludeTag tag filteredTagWithName =
  let
    ( filteredTag, name ) =
      filteredTagWithName
  in
    case filteredTag of
      Included t ->
        if t == tag then
          ( Excluded tag, name )
        else
          filteredTagWithName

      _ ->
        filteredTagWithName


includeTag : Tag -> FilteredTagWithName -> FilteredTagWithName
includeTag tag filteredTagWithName =
  let
    ( filteredTag, name ) =
      filteredTagWithName
  in
    case filteredTag of
      Excluded t ->
        if t == tag then
          ( Included tag, name )
        else
          filteredTagWithName

      _ ->
        filteredTagWithName


excludeAllTags : FilteredTagWithName -> FilteredTagWithName
excludeAllTags filteredTagWithName =
  let
    ( filteredTag, name ) =
      filteredTagWithName
  in
    case filteredTag of
      Excluded t ->
        ( Excluded t, name )

      Included t ->
        ( Excluded t, name )
