module ModelTest exposing (..)

import Conference
import Expect
import Model exposing (..)
import Date exposing (Month(..))
import FilteredTagSection exposing (FilteredTagSection)
import Tag exposing (Tag(..))
import Test exposing (describe, test)
import Time.Date exposing (date)


withTags : List FilteredTagSection -> Model
withTags tags =
    { conferences = []
    , currentDate = date 2016 1 1
    , includePastEvents = True
    , tags = tags
    }


blankModel : Model
blankModel =
    withTags []


tests =
    describe "Model"
        [ test "conferencesToShow filters out past events if includePastEvents is False" <|
            \() ->
                let
                    blankConference =
                        { name = ""
                        , link = ""
                        , startDate = date 1 1 1
                        , endDate = date 1 1 1
                        , location = ""
                        , cfpStartDate = Nothing
                        , cfpEndDate = Nothing
                        , tags = []
                        }

                    conference1 =
                        { blankConference | startDate = date 2015 12 30, name = "a" }

                    conference2 =
                        { blankConference | startDate = date 2016 1 2, name = "b" }

                    model =
                        { blankModel
                            | conferences = [ conference1, conference2 ]
                            , includePastEvents = False
                            , currentDate = date 2016 1 1
                        }
                in
                    conferencesToShow model
                        |> Expect.equal [ conference2 ]
        , test "shouldShow does not filter out past events if includePastEvents is True" <|
            \() ->
                let
                    blankConference =
                        { name = ""
                        , link = ""
                        , startDate = date 1 1 1
                        , endDate = date 1 1 1
                        , location = ""
                        , cfpStartDate = Nothing
                        , cfpEndDate = Nothing
                        , tags = []
                        }

                    conference1 =
                        { blankConference | startDate = date 2015 12 30, name = "a" }

                    conference2 =
                        { blankConference | startDate = date 2016 1 2, name = "b" }

                    model =
                        { blankModel
                            | conferences = [ conference1, conference2 ]
                            , includePastEvents = True
                            , currentDate = date 2016 1 1
                        }
                in
                    conferencesToShow model
                        |> Expect.equal [ conference1, conference2 ]
        ]
