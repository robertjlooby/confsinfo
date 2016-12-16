{-# LANGUAGE FlexibleContexts #-}

module RunAll where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql

import AddConference

migrateAll connStr =
    runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ do
            runMigration addConference
