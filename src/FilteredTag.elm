module FilteredTag (..) where

import Tag exposing (Tag)


type FilteredTag
  = Included Tag
  | Excluded Tag


getTag : FilteredTag -> Tag
getTag tag =
  case tag of
    Included t ->
      t

    Excluded t ->
      t


isIncluded : FilteredTag -> Bool
isIncluded tag =
  case tag of
    Included _ ->
      True

    Excluded _ ->
      False
