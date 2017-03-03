module ConferenceTest exposing (..)

import Conference exposing (..)
import Date exposing (Month(..))
import Expect
import Fuzz
import Tag exposing (Tag(..))
import TestHelpers exposing (..)
import Test exposing (describe, fuzz, fuzz2, fuzz3)
import Time.Date exposing (date)


blankConf : Conference.Model
blankConf =
    { name = ""
    , link = ""
    , startDate = date 1 1 1
    , endDate = date 1 1 1
    , location = ""
    , cfpStartDate = Nothing
    , cfpEndDate = Nothing
    , tags = []
    }


tests =
    describe "Conference"
        [ fuzz dateFuzzer "cfpStatus is (Closed, Nothing) if start and end are Nothing" <|
            \date ->
                cfpStatus date blankConf
                    |> Expect.equal ( Closed, Nothing )
        , fuzz dateFuzzer "cfpStatus is False if cfpEndDate is Nothing" <|
            \date ->
                cfpStatus date { blankConf | cfpStartDate = Just date }
                    |> Expect.equal ( Closed, Nothing )
        , fuzz dateFuzzer "cfpStatus is (Open, Just cfpEndDate) if cfpEndDate == currentDate" <|
            \date ->
                cfpStatus date { blankConf | cfpEndDate = Just date }
                    |> Expect.equal ( Open, Just date )
        ]
