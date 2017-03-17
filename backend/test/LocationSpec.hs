module LocationSpec where

import qualified Location
import Location (Location(..))
import Data.Maybe (fromJust)
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)

tests conn =
    describe "Location" $ do
        it "creates a location" $ do
            location <- Location.create conn $ Location Nothing "name" True

            shouldNotBe Nothing $ Location.id location
            shouldBe "name" $ Location.name location
            shouldBe True $ Location.approved location

        it "finds Nothing if location does not exist" $ do
            location <- Location.find conn 0

            shouldBe Nothing location

        it "can find a location by id" $ do
            location <- Location.create conn $ Location Nothing "name" True
            location' <- Location.find conn $ fromJust $ Location.id location

            shouldBe (Just location) location'

        it "can get all approved locations" $ do
            location1 <- Location.create conn $ Location Nothing "name" True
            location2 <- Location.create conn $ Location Nothing "other" True
            Location.create conn $ Location Nothing "third" False

            locations <- Location.findAll conn

            shouldBe [location1, location2] locations
