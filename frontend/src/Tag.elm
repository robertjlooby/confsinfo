module Tag exposing (..)


type Audience
    = Audience String


getAudienceName : Audience -> String
getAudienceName (Audience name) =
    name


type Language
    = Language String


getLanguageName : Language -> String
getLanguageName (Language name) =
    name


type Location
    = Location String


getLocationName : Location -> String
getLocationName (Location location) =
    location


type Topic
    = Topic String


getTopicName : Topic -> String
getTopicName (Topic topic) =
    topic
