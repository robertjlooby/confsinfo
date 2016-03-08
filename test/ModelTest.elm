module ModelTest (..) where

import ElmTest exposing (assertEqual, suite, test)
import Date
import Model exposing (..)
import ModelInternal exposing (..)
import Date exposing (Month(..))
import FilteredTagSection
import Tag exposing (Tag(..))


withTags : List FilteredTagSection.Model -> ModelInternal.Model
withTags tags =
  { conferences = []
  , currentDate = ( 2016, Jan, 1 )
  , includePastEvents = True
  , tags = tags
  }


blankModel : ModelInternal.Model
blankModel =
  withTags []


tests =
  suite
    "Model"
    [ test "conferencesToShow filters out past events if includePastEvents is False"
        <| let
            blankConference =
              { name = ""
              , link = ""
              , startDate = ( 1, Date.Jan, 1 )
              , endDate = ( 1, Date.Jan, 1 )
              , location = ""
              , cfpStartDate = Nothing
              , cfpEndDate = Nothing
              , tags = []
              }

            conference1 =
              { blankConference | startDate = ( 2015, Date.Dec, 30 ) }

            conference2 =
              { blankConference | startDate = ( 2016, Date.Jan, 2 ) }

            model =
              { blankModel
                | conferences = [ conference1, conference2 ]
                , includePastEvents = False
                , currentDate = ( 2016, Jan, 1 )
              }
           in
            assertEqual [ conference2 ] <| conferencesToShow model
    , test "shouldShow does not filter out past events if includePastEvents is True"
        <| let
            blankConference =
              { name = ""
              , link = ""
              , startDate = ( 1, Date.Jan, 1 )
              , endDate = ( 1, Date.Jan, 1 )
              , location = ""
              , cfpStartDate = Nothing
              , cfpEndDate = Nothing
              , tags = []
              }

            conference1 =
              { blankConference | startDate = ( 2015, Date.Dec, 30 ) }

            conference2 =
              { blankConference | startDate = ( 2016, Date.Jan, 2 ) }

            model =
              { blankModel
                | conferences = [ conference1, conference2 ]
                , includePastEvents = True
                , currentDate = ( 2016, Jan, 1 )
              }
           in
            assertEqual [ conference1, conference2 ] <| conferencesToShow model
    ]
