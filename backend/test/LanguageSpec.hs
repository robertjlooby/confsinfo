module LanguageSpec where

import qualified Language
import Language (Language(..))
import Data.Maybe (fromJust)
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)

tests conn =
    describe "Language" $ do
        it "creates a language" $ do
            language <- Language.create conn $ Language Nothing "name" True

            shouldNotBe Nothing $ Language.id language
            shouldBe "name" $ Language.name language
            shouldBe True $ Language.approved language

        it "finds Nothing if language does not exist" $ do
            language <- Language.find conn 0

            shouldBe Nothing language

        it "can find a language by id" $ do
            language <- Language.create conn $ Language Nothing "name" True
            language' <- Language.find conn $ fromJust $ Language.id language

            shouldBe (Just language) language'

        it "can get all approved languages" $ do
            language1 <- Language.create conn $ Language Nothing "name" True
            language2 <- Language.create conn $ Language Nothing "other" True
            Language.create conn $ Language Nothing "third" False

            languages <- Language.findAll conn

            shouldBe [language1, language2] languages
