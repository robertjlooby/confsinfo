module TopicSpec where

import qualified Topic
import Topic (Topic(..))
import Data.Maybe (fromJust)
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)

tests conn =
    describe "Topic" $ do
        it "creates a topic" $ do
            topic <- Topic.create conn $ Topic Nothing "name" True

            shouldNotBe Nothing $ Topic.id topic
            shouldBe "name" $ Topic.name topic
            shouldBe True $ Topic.approved topic

        it "finds Nothing if topic does not exist" $ do
            topic <- Topic.find conn 0

            shouldBe Nothing topic

        it "can find a topic by id" $ do
            topic <- Topic.create conn $ Topic Nothing "name" True
            topic' <- Topic.find conn $ fromJust $ Topic.id topic

            shouldBe (Just topic) topic'

        it "can get all approved topics" $ do
            topic1 <- Topic.create conn $ Topic Nothing "name" True
            topic2 <- Topic.create conn $ Topic Nothing "other" True
            Topic.create conn $ Topic Nothing "third" False

            topics <- Topic.findAll conn

            shouldBe [topic1, topic2] topics
