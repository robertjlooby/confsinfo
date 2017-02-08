module RunMigrations where

import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationCommand(..), MigrationContext(..), MigrationResult, runMigration)
import Util (getConn)

runAllMigrations :: Connection -> IO (MigrationResult String)
runAllMigrations conn =
    withTransaction conn $ do
        runMigration $ MigrationContext MigrationInitialization True conn
        runMigration $ MigrationContext (MigrationDirectory "db/migrations") True conn
