module FilteredTagSectionInternal (..) where

import FilteredTag
import Tag exposing (Tag)


-- Update


type Action
  = UpdateTag Tag FilteredTag.Action
  | Reset
