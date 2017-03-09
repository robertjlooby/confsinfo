module ModelTest exposing (..)

import Expect
import FilteredTag exposing (FilteredTag, State(..))
import Model exposing (..)
import Tag exposing (..)
import Test exposing (describe, test)


model : Bool -> List (FilteredTag Audience) -> List (FilteredTag Language) -> List (FilteredTag Location) -> List (FilteredTag Topic) -> Model
model includePast audiences langs locations topics =
    { conferences = []
    , includePastEvents = includePast
    , audiences = { sectionName = "audiences", tags = audiences }
    , languages = { sectionName = "langs", tags = langs }
    , locations = { sectionName = "locations", tags = locations }
    , topics = { sectionName = "topics", tags = topics }
    }


emptyModel =
    model False [] [] [] []


tests =
    describe "Model"
        [ describe "generateQueryString"
            [ test "with an empty model" <|
                \() ->
                    generateQueryString emptyModel
                        |> Expect.equal "?"
            , test "with an audience" <|
                \() ->
                    model False [ FilteredTag (Audience "Developers") Included ] [] [] []
                        |> generateQueryString
                        |> Expect.equal "?audience=Developers"
            , test "with multiple audiences" <|
                \() ->
                    [ FilteredTag (Audience "Developers") Included
                    , FilteredTag (Audience "Designers") Included
                    , FilteredTag (Audience "Suits") Excluded
                    ]
                        |> (\audience -> model False audience [] [] [])
                        |> generateQueryString
                        |> Expect.equal "?audience=Developers&audience=Designers"
            , test "with a language" <|
                \() ->
                    model False [] [ FilteredTag (Language "English") Included ] [] []
                        |> generateQueryString
                        |> Expect.equal "?language=English"
            , test "with multiple languages" <|
                \() ->
                    [ FilteredTag (Language "English") Included
                    , FilteredTag (Language "Spanish") Included
                    , FilteredTag (Language "Shyriiwook") Excluded
                    ]
                        |> (\langs -> model False [] langs [] [])
                        |> generateQueryString
                        |> Expect.equal "?language=English&language=Spanish"
            , test "with a location" <|
                \() ->
                    model False [] [] [ FilteredTag (Location "USA") Included ] []
                        |> generateQueryString
                        |> Expect.equal "?location=USA"
            , test "with multiple locations" <|
                \() ->
                    [ FilteredTag (Location "USA") Included
                    , FilteredTag (Location "Mexico") Included
                    , FilteredTag (Location "Alderaan") Excluded
                    ]
                        |> (\locations -> model False [] [] locations [])
                        |> generateQueryString
                        |> Expect.equal "?location=USA&location=Mexico"
            , test "with a topic" <|
                \() ->
                    model False [] [] [] [ FilteredTag (Topic "Elm") Included ]
                        |> generateQueryString
                        |> Expect.equal "?topic=Elm"
            , test "with multiple topics" <|
                \() ->
                    [ FilteredTag (Topic "Elm") Included
                    , FilteredTag (Topic "Haskell") Included
                    , FilteredTag (Topic "JavaScript") Excluded
                    ]
                        |> model False [] [] []
                        |> generateQueryString
                        |> Expect.equal "?topic=Elm&topic=Haskell"
            , test "with include past events" <|
                \() ->
                    model True [] [] [] []
                        |> generateQueryString
                        |> Expect.equal "?includePastEvents=True"
            , test "with all" <|
                \() ->
                    model
                        True
                        [ FilteredTag (Audience "Developers") Excluded
                        , FilteredTag (Audience "Designers") Included
                        ]
                        [ FilteredTag (Language "English") Included
                        , FilteredTag (Language "Spanish") Included
                        , FilteredTag (Language "Shyriiwook") Excluded
                        ]
                        [ FilteredTag (Location "USA") Included
                        , FilteredTag (Location "Mexico") Included
                        , FilteredTag (Location "Alderaan") Excluded
                        ]
                        [ FilteredTag (Topic "Elm") Included
                        , FilteredTag (Topic "Haskell") Included
                        , FilteredTag (Topic "JavaScript") Excluded
                        ]
                        |> generateQueryString
                        |> Expect.equal "?audience=Designers&includePastEvents=True&language=English&language=Spanish&location=USA&location=Mexico&topic=Elm&topic=Haskell"
            ]
        , describe "init"
            [ test "does not include past events by default" <|
                \() ->
                    init emptyModel { search = "" }
                        |> Tuple.first
                        |> .includePastEvents
                        |> Expect.equal False
            , test "sets include past events" <|
                \() ->
                    init emptyModel { search = "?includePastEvents=True" }
                        |> Tuple.first
                        |> .includePastEvents
                        |> Expect.equal True
            , test "does not include any audience by default" <|
                \() ->
                    init emptyModel { search = "" }
                        |> Tuple.first
                        |> includedAudiences
                        |> Expect.equal []
            , test "sets included audience" <|
                \() ->
                    init (model False [ FilteredTag (Audience "developers") Excluded ] [] [] []) { search = "?audience=developers" }
                        |> Tuple.first
                        |> includedAudiences
                        |> Expect.equal [ Audience "developers" ]
            , test "does not include any languages by default" <|
                \() ->
                    init emptyModel { search = "" }
                        |> Tuple.first
                        |> includedLanguages
                        |> Expect.equal []
            , test "sets included languages" <|
                \() ->
                    init (model False [] [ FilteredTag (Language "english") Excluded ] [] []) { search = "?language=english" }
                        |> Tuple.first
                        |> includedLanguages
                        |> Expect.equal [ Language "english" ]
            , test "does not include any locations by default" <|
                \() ->
                    init emptyModel { search = "" }
                        |> Tuple.first
                        |> includedLocations
                        |> Expect.equal []
            , test "sets included locations" <|
                \() ->
                    init (model False [] [] [ FilteredTag (Location "USA") Excluded ] []) { search = "?location=USA" }
                        |> Tuple.first
                        |> includedLocations
                        |> Expect.equal [ Location "USA" ]
            , test "does not include any topics by default" <|
                \() ->
                    init emptyModel { search = "" }
                        |> Tuple.first
                        |> includedTopics
                        |> Expect.equal []
            , test "sets included topics" <|
                \() ->
                    init (model False [] [] [] [ FilteredTag (Topic "elm") Excluded ]) { search = "?topic=elm" }
                        |> Tuple.first
                        |> includedTopics
                        |> Expect.equal [ Topic "elm" ]
            ]
        ]
