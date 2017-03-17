module ModelTest exposing (..)

import Expect
import FilteredTag exposing (FilteredTag)
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
                    model False [ FilteredTag (Audience "Developers") True ] [] [] []
                        |> generateQueryString
                        |> Expect.equal "?audience=Developers"
            , test "with multiple audiences" <|
                \() ->
                    [ FilteredTag (Audience "Developers") True
                    , FilteredTag (Audience "Designers") True
                    , FilteredTag (Audience "Suits") False
                    ]
                        |> (\audience -> model False audience [] [] [])
                        |> generateQueryString
                        |> Expect.equal "?audience=Developers&audience=Designers"
            , test "with a language" <|
                \() ->
                    model False [] [ FilteredTag (Language "English") True ] [] []
                        |> generateQueryString
                        |> Expect.equal "?language=English"
            , test "with multiple languages" <|
                \() ->
                    [ FilteredTag (Language "English") True
                    , FilteredTag (Language "Spanish") True
                    , FilteredTag (Language "Shyriiwook") False
                    ]
                        |> (\langs -> model False [] langs [] [])
                        |> generateQueryString
                        |> Expect.equal "?language=English&language=Spanish"
            , test "with a location" <|
                \() ->
                    model False [] [] [ FilteredTag (Location "USA") True ] []
                        |> generateQueryString
                        |> Expect.equal "?location=USA"
            , test "with multiple locations" <|
                \() ->
                    [ FilteredTag (Location "USA") True
                    , FilteredTag (Location "Mexico") True
                    , FilteredTag (Location "Alderaan") False
                    ]
                        |> (\locations -> model False [] [] locations [])
                        |> generateQueryString
                        |> Expect.equal "?location=USA&location=Mexico"
            , test "with a topic" <|
                \() ->
                    model False [] [] [] [ FilteredTag (Topic "Elm") True ]
                        |> generateQueryString
                        |> Expect.equal "?topic=Elm"
            , test "with multiple topics" <|
                \() ->
                    [ FilteredTag (Topic "Elm") True
                    , FilteredTag (Topic "Haskell") True
                    , FilteredTag (Topic "JavaScript") False
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
                        [ FilteredTag (Audience "Developers") False
                        , FilteredTag (Audience "Designers") True
                        ]
                        [ FilteredTag (Language "English") True
                        , FilteredTag (Language "Spanish") True
                        , FilteredTag (Language "Shyriiwook") False
                        ]
                        [ FilteredTag (Location "USA") True
                        , FilteredTag (Location "Mexico") True
                        , FilteredTag (Location "Alderaan") False
                        ]
                        [ FilteredTag (Topic "Elm") True
                        , FilteredTag (Topic "Haskell") True
                        , FilteredTag (Topic "JavaScript") False
                        ]
                        |> generateQueryString
                        |> Expect.equal "?audience=Designers&includePastEvents=True&language=English&language=Spanish&location=USA&location=Mexico&topic=Elm&topic=Haskell"
            ]
        ]
