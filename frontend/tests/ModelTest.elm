module ModelTest exposing (..)

import Expect
import FilteredTag exposing (FilteredTag, State(..))
import Model exposing (..)
import Tag exposing (..)
import Test exposing (describe, test)


model : Bool -> List (FilteredTag Language) -> List (FilteredTag Tag) -> Model
model includePast langs tags =
    { conferences = []
    , includePastEvents = includePast
    , languages = { sectionName = "langs", tags = langs }
    , tags = [ { sectionName = "tags", tags = tags } ]
    }


emptyModel =
    model False [] []


tests =
    describe "Model"
        [ describe "generateQueryString"
            [ test "with no languages or tags" <|
                \() ->
                    generateQueryString emptyModel
                        |> Expect.equal "?"
            , test "with a language" <|
                \() ->
                    model False [ FilteredTag (Language "English") Included ] []
                        |> generateQueryString
                        |> Expect.equal "?language=English"
            , test "with multiple languages" <|
                \() ->
                    [ FilteredTag (Language "English") Included
                    , FilteredTag (Language "Spanish") Included
                    , FilteredTag (Language "Shyriiwook") Excluded
                    ]
                        |> (\langs -> model False langs [])
                        |> generateQueryString
                        |> Expect.equal "?language=English&language=Spanish"
            , test "with a tag" <|
                \() ->
                    model False [] [ FilteredTag (Tag "Elm") Included ]
                        |> generateQueryString
                        |> Expect.equal "?tag=Elm"
            , test "with multiple tags" <|
                \() ->
                    [ FilteredTag (Tag "Elm") Included
                    , FilteredTag (Tag "Haskell") Included
                    , FilteredTag (Tag "JavaScript") Excluded
                    ]
                        |> model False []
                        |> generateQueryString
                        |> Expect.equal "?tag=Elm&tag=Haskell"
            , test "with include past events" <|
                \() ->
                    model True [] []
                        |> generateQueryString
                        |> Expect.equal "?includePastEvents=True"
            , test "with all" <|
                \() ->
                    [ FilteredTag (Tag "Elm") Included
                    , FilteredTag (Tag "Haskell") Included
                    , FilteredTag (Tag "JavaScript") Excluded
                    ]
                        |> model True
                            [ FilteredTag (Language "English") Included
                            , FilteredTag (Language "Spanish") Included
                            , FilteredTag (Language "Shyriiwook") Excluded
                            ]
                        |> generateQueryString
                        |> Expect.equal "?includePastEvents=True&language=English&language=Spanish&tag=Elm&tag=Haskell"
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
            , test "does not include any languages by default" <|
                \() ->
                    init emptyModel { search = "" }
                        |> Tuple.first
                        |> includedLanguages
                        |> Expect.equal []
            , test "sets included languages" <|
                \() ->
                    init (model False [ FilteredTag (Language "english") Excluded ] []) { search = "?language=english" }
                        |> Tuple.first
                        |> includedLanguages
                        |> Expect.equal [ Language "english" ]
            , test "does not include any tags by default" <|
                \() ->
                    init emptyModel { search = "" }
                        |> Tuple.first
                        |> includedTags
                        |> Expect.equal []
            , test "sets included tags" <|
                \() ->
                    init (model False [] [ FilteredTag (Tag "elm") Excluded ]) { search = "?tag=elm" }
                        |> Tuple.first
                        |> includedTags
                        |> Expect.equal [ Tag "elm" ]
            ]
        ]
