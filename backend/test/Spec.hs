import Configuration.Dotenv (loadFile)
import Database.PostgreSQL.Simple.Util (withTransactionRolledBack)
import RunMigrations (runAllMigrations)
import Test.Hspec (around_, describe, hspec)
import Util (getConn)

import qualified ConferenceSpec

main :: IO ()
main = do
    loadFile False "config/config.env"
    conn <- getConn
    runAllMigrations conn
    hspec $
        around_ (withTransactionRolledBack conn) $
            describe "All tests" $ do
                ConferenceSpec.tests conn
