module FilteredTagSectionInternal exposing (..)

import FilteredTag
import Tag exposing (Tag)


-- Update


type Msg
  = UpdateTag Tag FilteredTag.Msg
  | Reset
