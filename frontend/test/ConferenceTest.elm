module ConferenceTest exposing (..)

import Conference exposing (..)
import Date exposing (Month(..))
import DaTuple as DT
import Expect
import Fuzz
import Tag exposing (Tag(..))
import TestHelpers exposing (..)
import Test exposing (describe, fuzz, fuzz2, fuzz3)


blankConf : Conference.Model
blankConf =
    { name = ""
    , link = ""
    , startDate = ( 1, Jan, 1 )
    , endDate = ( 1, Jan, 1 )
    , location = ""
    , cfpStartDate = Nothing
    , cfpEndDate = Nothing
    , tags = []
    }


tests =
    describe "Conference"
        [ fuzz daTupleFuzzer "cfpStatus is (Closed, Nothing) if start and end are Nothing" <|
            \date ->
                cfpStatus date blankConf
                    |> Expect.equal ( Closed, Nothing )
        , fuzz daTupleFuzzer "cfpStatus is False if cfpEndDate is Nothing" <|
            \date ->
                cfpStatus date { blankConf | cfpStartDate = Just date }
                    |> Expect.equal ( Closed, Nothing )
        , fuzz daTupleFuzzer "cfpStatus is (Open, Just cfpEndDate) if cfpEndDate == currentDate" <|
            \date ->
                cfpStatus date { blankConf | cfpEndDate = Just date }
                    |> Expect.equal ( Open, Just date )
        , fuzz2 daTupleFuzzer daTupleFuzzer "compareConferences sorts first by start date" <|
            \d1 d2 ->
                compareConferences { blankConf | startDate = d1 } { blankConf | startDate = d2 }
                    |> Expect.equal (DT.compareDaTuples d1 d2)
        , fuzz3 daTupleFuzzer Fuzz.string Fuzz.string "compareConferences sorts by name if the start date is the same" <|
            \d n1 n2 ->
                compareConferences { blankConf | name = n1, startDate = d } { blankConf | name = n2, startDate = d }
                    |> Expect.equal (compare n1 n2)
        ]
