module ConferenceSpec where

import qualified Conference as C
import Data.Maybe (fromJust)
import Data.Time.Calendar (fromGregorian)
import Test.Hspec (describe, hspec, it, shouldBe)

tests conn =
    describe "Conference" $ do
        it "creates a conference" $
            let startDate = fromGregorian 2016 1 15
                endDate = fromGregorian 2016 1 20
                cfpEndDate = Just $ fromGregorian 2015 12 31
                conference = C.Conference Nothing "name" "url" startDate endDate Nothing cfpEndDate True
            in do
                newConference <- C.create conn conference
                let conferenceWithId = conference { C.id = C.id newConference }
                shouldBe conferenceWithId newConference

        it "finds Nothing if conference does not exist" $ do
            conference <- C.find conn 0
            shouldBe Nothing conference

        it "can find a conference by id" $
            let startDate = fromGregorian 2016 2 15
                endDate = fromGregorian 2016 2 20
                cfpEndDate = Just $ fromGregorian 2015 12 31
            in do
                conference <- C.create conn $ C.Conference Nothing "name" "url" startDate endDate Nothing cfpEndDate True
                conference' <- C.find conn $ fromJust $ C.id conference
                shouldBe (Just conference) conference'

        it "can get all approved conferences" $
            let startDate = fromGregorian 2016 3 15
                endDate = fromGregorian 2016 3 20
                cfpEndDate = Just $ fromGregorian 2015 12 31
            in do
                conference1 <- C.create conn $ C.Conference Nothing "name1" "url1" startDate endDate Nothing cfpEndDate True
                conference2 <- C.create conn $ C.Conference Nothing "name2" "url2" startDate endDate Nothing cfpEndDate True
                C.create conn $ C.Conference Nothing "name2" "url2" startDate endDate Nothing cfpEndDate False
                conferences <- C.findAll conn
                shouldBe [conference1, conference2] conferences
