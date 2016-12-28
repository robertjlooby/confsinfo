{-# LANGUAGE FlexibleContexts #-}

module RunAll where

import Control.Monad.Reader (MonadIO, ReaderT)
import Database.Persist.Postgresql (SqlBackend, runMigration)

import AddConference

migrateAll :: (MonadIO m) => ReaderT SqlBackend m ()
migrateAll =
    runMigration addConference
