{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AddConference (Conference)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Database.Persist.Class (insert, selectList)
import Database.Persist.Postgresql (runSqlConn, SqlBackend, withPostgresqlConn)
import Database.Persist.Types (Filter)
import Network.Wai.Middleware.RequestLogger (autoFlush, outputFormat, mkRequestLogger, OutputFormat( Apache ), IPAddrSource( FromHeader ))
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import RunAll (migrateAll)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import Web.Scotty

main :: IO ()
main =
    let
      connStr = pack $ unsafePerformIO $ getEnv "DATABASE_URL"
      runDB :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => ReaderT SqlBackend m a -> m a
      runDB query = withPostgresqlConn connStr (runSqlConn query)
      getPort :: IO Int
      getPort = fmap read (getEnv "PORT")
    in
      do
        runStdoutLoggingT $ runDB migrateAll
        port <- getPort
        requestLogger <- mkRequestLogger def { outputFormat = Apache FromHeader, autoFlush = False }
        scotty port $ do
          middleware requestLogger
          middleware $ staticPolicy (addBase "dist")

          get "/" $ do
              setHeader "Content-Type" "text/html"
              file "dist/index.html"

          get "/conferences" $ do
              conferences <- runStdoutLoggingT $ runDB $ selectList ([] :: [Filter Conference]) []
              json conferences

          post "/conferences" $ do
              let getConference :: ActionM Conference
                  getConference = jsonData
              conference <- getConference
              id <- runStdoutLoggingT $ runDB $ insert conference
              json id

          get "/health" $ do
              text "OK"
