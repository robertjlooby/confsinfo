module Tag exposing (..)


type Language
    = Language String


getLanguageName : Language -> String
getLanguageName (Language name) =
    name


type Tag
    = Tag String


getTagName : Tag -> String
getTagName (Tag tag) =
    tag
