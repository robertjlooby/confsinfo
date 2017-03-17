import Configuration.Dotenv (loadFile)
import Database.PostgreSQL.Simple.Util (withTransactionRolledBack)
import RunMigrations (runAllMigrations)
import Test.Hspec (around_, describe, hspec)
import Util (getConn)

import qualified AudienceSpec
import qualified ConferenceSpec
import qualified InitialModelSpec
import qualified LanguageSpec
import qualified LocationSpec
import qualified TopicSpec

main :: IO ()
main = do
    loadFile False "config/config.env"
    conn <- getConn
    runAllMigrations conn
    hspec $
        around_ (withTransactionRolledBack conn) $
            describe "All tests" $ do
                AudienceSpec.tests conn
                ConferenceSpec.tests conn
                InitialModelSpec.tests conn
                LanguageSpec.tests conn
                LocationSpec.tests conn
                TopicSpec.tests conn
