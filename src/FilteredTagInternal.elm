module FilteredTagInternal (..) where

-- Model


type State
  = Included
  | Excluded



-- Update


type Action
  = Include
  | Exclude
