module InitialModelSpec where

import qualified Audience
import Audience (Audience(..))
import qualified Conference
import Conference (Conference(..))
import qualified InitialModel
import InitialModel (InitialModel(..))
import qualified Language
import Language (Language(..))
import qualified Location
import Location (Location(..))
import qualified Topic
import Topic (Topic(..))
import Data.Time.Calendar (fromGregorian)
import Test.Hspec (describe, hspec, it, shouldBe)

tests conn =
    describe "InitialModel" $ do
        it "includes all approved conferences data" $
            let startDate = fromGregorian 2016 1 15
                endDate = fromGregorian 2016 1 20
            in do
                Conference.create conn $ Conference Nothing "conf 1" "url" startDate endDate Nothing Nothing True
                Conference.create conn $ Conference Nothing "conf 2" "url" startDate endDate Nothing Nothing True
                Conference.create conn $ Conference Nothing "conf 3" "url" startDate endDate Nothing Nothing False

                Audience.create conn $ Audience Nothing "aud 2" True
                Audience.create conn $ Audience Nothing "aud 3" False
                Audience.create conn $ Audience Nothing "aud 1" True

                Language.create conn $ Language Nothing "lang 2" True
                Language.create conn $ Language Nothing "lang 3" False
                Language.create conn $ Language Nothing "lang 1" True

                Location.create conn $ Location Nothing "loc 2" True
                Location.create conn $ Location Nothing "loc 3" False
                Location.create conn $ Location Nothing "loc 1" True

                Topic.create conn $ Topic Nothing "topic 2" True
                Topic.create conn $ Topic Nothing "topic 3" False
                Topic.create conn $ Topic Nothing "topic 1" True

                initialModel <- InitialModel.find conn
                shouldBe False $ InitialModel.includePastEvents initialModel
                shouldBe ["aud 1", "aud 2"] $ InitialModel.audiences initialModel
                shouldBe ["lang 1", "lang 2"] $ InitialModel.languages initialModel
                shouldBe ["loc 1", "loc 2"] $ InitialModel.locations initialModel
                shouldBe ["topic 1", "topic 2"] $ InitialModel.topics initialModel

