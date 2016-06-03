module FilteredTagInternal exposing (..)

-- Model


type State
  = Included
  | Excluded



-- Update


type Msg
  = Include
  | Exclude
