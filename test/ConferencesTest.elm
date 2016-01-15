module ConferencesTest (..) where

import ElmTest exposing (assertEqual, suite, test)
import Date
import Conferences exposing (..)


tests =
    suite
        "Conferences"
        [ test "update exclude excludes a tag"
            <| let
                tags = [ ( Included DotNet, "" ), ( Included Ruby, "" ) ]

                model = { conferences = [], tags = [ ( "", tags ) ] }
               in
                assertEqual { conferences = [], tags = [ ( "", [ ( Included DotNet, "" ), ( Excluded Ruby, "" ) ] ) ] } <| update (Exclude Ruby) model
        , test "update include includes a tag"
            <| let
                tags = [ ( Excluded DotNet, "" ), ( Included Ruby, "" ) ]

                model = { conferences = [], tags = [ ( "", tags ) ] }
               in
                assertEqual { conferences = [], tags = [ ( "", [ ( Included DotNet, "" ), ( Included Ruby, "" ) ] ) ] } <| update (Include DotNet) model
        , test "update reset excludes all tags"
            <| let
                tags = [ ( Included Agile, "" ), ( Excluded DotNet, "" ), ( Included Ruby, "" ) ]

                model = { conferences = [], tags = [ ( "", tags ) ] }
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

                model = { conferences = [], tags = tags }
               in
                assertEqual { model | tags = newTags } <| initialize [ toString Agile, toString Ruby, "Other" ] model
        , test "included tags gets just string versions of all included tags"
            <| let
                tags = [ ( "", [ ( Included Agile, "1" ), ( Excluded DotNet, "2" ) ] ), ( "", [ ( Included Ruby, "3" ) ] ) ]

                model = { conferences = [], tags = tags }
               in
                assertEqual [ toString Agile, toString Ruby ] <| includedTags model
        ]
