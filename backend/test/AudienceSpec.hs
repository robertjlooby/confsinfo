module AudienceSpec where

import qualified Audience
import Audience (Audience(..))
import Data.Maybe (fromJust)
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)

tests conn =
    describe "Audience" $ do
        it "creates a audience" $ do
            audience <- Audience.create conn $ Audience Nothing "name" True

            shouldNotBe Nothing $ Audience.id audience
            shouldBe "name" $ Audience.name audience
            shouldBe True $ Audience.approved audience

        it "finds Nothing if audience does not exist" $ do
            audience <- Audience.find conn 0

            shouldBe Nothing audience

        it "can find a audience by id" $ do
            audience <- Audience.create conn $ Audience Nothing "name" True
            audience' <- Audience.find conn $ fromJust $ Audience.id audience

            shouldBe (Just audience) audience'

        it "can get all approved audiences" $ do
            audience1 <- Audience.create conn $ Audience Nothing "name" True
            audience2 <- Audience.create conn $ Audience Nothing "other" True
            Audience.create conn $ Audience Nothing "third" False

            audiences <- Audience.findAll conn

            shouldBe [audience1, audience2] audiences
