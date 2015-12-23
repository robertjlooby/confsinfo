module ConferencesTest where

import ElmTest exposing (assertEqual, suite, test)

import Conferences exposing (..)
import DateFormatter exposing (parseDate)


tests =
  suite
    "Conferences"
    [
      test "update exclude excludes a tag" <|
        let
          tags = [(Included DotNet, ""), (Included Ruby, "")]
          model = { conferences = [], tags = [("", tags)] }
        in
          assertEqual { conferences = [], tags = [("", [(Included DotNet, ""), (Excluded Ruby, "")])] } <| update (Exclude Ruby) model
    , test "update include includes a tag" <|
        let
          tags = [(Excluded DotNet, ""), (Included Ruby, "")]
          model = { conferences = [], tags = [("", tags)] }
        in
          assertEqual { conferences = [], tags = [("", [(Included DotNet, ""), (Included Ruby, "")])] } <| update (Include DotNet) model
    , test "update reset excludes all tags" <|
        let
          tags = [(Included Agile, ""), (Excluded DotNet, ""), (Included Ruby, "")]
          model = { conferences = [], tags = [("", tags)] }
        in
          assertEqual { model | tags = [("", [(Excluded Agile, ""), (Excluded DotNet, ""), (Excluded Ruby, "")])] } <| update Reset model
    , test "should show conference if it has all included tags" <|
        let
          tags = [Included Agile, Excluded DotNet, Included Ruby]
          blankConference = { name = "", link = "", startDate = parseDate "", endDate = parseDate "", location = "", tags = [] }
          conference1 = { blankConference | tags = [ Agile, Ruby ] }
          conference2 = { blankConference | tags = [ Agile, DotNet ] }
          conference3 = { blankConference | tags = [ DotNet, Ruby ] }
          conference4 = { blankConference | tags = [ Agile, DotNet, Ruby ] }
          conferences = [ conference1, conference2, conference3, conference4 ]
        in
           assertEqual [ conference1, conference4 ] <| shouldShow tags conferences
    ]
