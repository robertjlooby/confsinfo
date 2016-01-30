module ModelTest (..) where

import ElmTest exposing (assertEqual, suite, test)
import Date
import Model exposing (..)
import Date exposing (Month(..))
import FilteredTag exposing (FilteredTag(..))
import Tag exposing (Tag(..))


withTags : List ( String, List ( FilteredTag, String ) ) -> Model
withTags tags =
    { conferences = []
    , currentDate = ( 2016, Jan, 1 )
    , includePastEvents = True
    , tags = tags
    }


tests =
    suite
        "Model"
        [ test "update exclude excludes a tag"
            <| let
                tags = [ ( Included DotNet, "" ), ( Included Ruby, "" ) ]

                model = withTags [ ( "", tags ) ]
               in
                assertEqual (withTags [ ( "", [ ( Included DotNet, "" ), ( Excluded Ruby, "" ) ] ) ]) <| update (Exclude Ruby) model
        , test "update include includes a tag"
            <| let
                tags = [ ( Excluded DotNet, "" ), ( Included Ruby, "" ) ]

                model = withTags [ ( "", tags ) ]
               in
                assertEqual (withTags [ ( "", [ ( Included DotNet, "" ), ( Included Ruby, "" ) ] ) ]) <| update (Include DotNet) model
        , test "update reset excludes all tags"
            <| let
                tags = [ ( Included Agile, "" ), ( Excluded DotNet, "" ), ( Included Ruby, "" ) ]

                model = withTags [ ( "", tags ) ]
               in
                assertEqual { model | tags = [ ( "", [ ( Excluded Agile, "" ), ( Excluded DotNet, "" ), ( Excluded Ruby, "" ) ] ) ] } <| update Reset model
        , test "should show conference if it has all included tags"
            <| let
                tags = [ Included Agile, Excluded DotNet, Included Ruby ]

                blankConference = { name = "", link = "", startDate = ( 1, Date.Jan, 1 ), endDate = ( 1, Date.Jan, 1 ), location = "", tags = [] }

                conference1 = { blankConference | tags = [ Agile, Ruby ] }

                conference2 = { blankConference | tags = [ Agile, DotNet ] }

                conference3 = { blankConference | tags = [ DotNet, Ruby ] }

                conference4 = { blankConference | tags = [ Agile, DotNet, Ruby ] }

                conferences = [ conference1, conference2, conference3, conference4 ]
               in
                assertEqual [ conference1, conference4 ] <| shouldShow tags conferences
        , test "initialize includes all tags from their string versions"
            <| let
                tags = [ ( "", [ ( Excluded Agile, "1" ), ( Excluded DotNet, "2" ) ] ), ( "", [ ( Excluded Ruby, "3" ) ] ) ]

                newTags = [ ( "", [ ( Included Agile, "1" ), ( Excluded DotNet, "2" ) ] ), ( "", [ ( Included Ruby, "3" ) ] ) ]

                model = withTags tags
               in
                assertEqual { model | tags = newTags } <| initialize [ toString Agile, toString Ruby, "Other" ] model
        , test "included tags gets just string versions of all included tags"
            <| let
                tags = [ ( "", [ ( Included Agile, "1" ), ( Excluded DotNet, "2" ) ] ), ( "", [ ( Included Ruby, "3" ) ] ) ]

                model = withTags tags
               in
                assertEqual [ toString Agile, toString Ruby ] <| includedTags model
        , test "initial model filters out past events"
            <| assertEqual False initialState.includePastEvents
        , test "update IncludePastEvents can uninclude past events"
            <| let
                blankModel = (withTags [])

                model = { blankModel | includePastEvents = True }

                newModel = update (IncludePastEvents False) model
               in
                assertEqual False newModel.includePastEvents
        , test "update IncludePastEvents can include past events"
            <| let
                blankModel = (withTags [])

                model = { blankModel | includePastEvents = False }

                newModel = update (IncludePastEvents True) model
               in
                assertEqual True newModel.includePastEvents
        , test "initial model has currentDate at Jan 1, 2016"
            <| assertEqual ( 2016, Jan, 1 ) initialState.currentDate
        , test "update SetCurrentDate sets the currentDate"
            <| let
                blankModel = (withTags [])

                model = { blankModel | currentDate = ( 2016, Jan, 1 ) }

                newModel = update (SetCurrentDate 1446595200000) model

                ( y, m, d ) = newModel.currentDate
               in
                assertEqual True <| y == 2015 && m == Nov && (d <= 5 || d >= 3)
        ]
