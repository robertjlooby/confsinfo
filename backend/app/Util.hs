module Util (getConn, getPort) where

import Data.ByteString.Char8 (ByteString, pack)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import System.Environment (getEnv)

getConn :: IO Connection
getConn = do
    connStr <- getConnStr
    connectPostgreSQL connStr

getConnStr :: IO ByteString
getConnStr = pack <$> getEnv "DATABASE_URL"

getPort :: IO Int
getPort = fmap read (getEnv "PORT")
