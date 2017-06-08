{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (loadFile)
import Conference (create, findAll)
import Data.ByteString.Char8 (isPrefixOf)
import Data.Default (def)
import Network.Wai (ifRequest, requestHeaderHost)
import Network.Wai.Middleware.ForceSSL
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import RunMigrations (runAllMigrations)
import Util (getConn, getPort)
import Web.Scotty

main :: IO ()
main = do
    loadFile False "config/config.env"
    conn <- getConn
    runAllMigrations conn
    port <- getPort
    scotty port $ do
        middleware logStdout
        middleware $ ifRequest isNotLocal forceSSL
        middleware $ staticPolicy (addBase "dist")

        get "/" $ do
            setHeader "Content-Type" "text/html"
            file "dist/index.html"

        get "/conferences" $ do
            conferences <- liftAndCatchIO $ findAll conn
            json conferences

        post "/conferences" $ do
            conference <- jsonData
            conference' <- liftAndCatchIO $ create conn conference
            json conference'
    where
        isNotLocal req =
            case requestHeaderHost req of
                Just host -> not $ "localhost" `isPrefixOf` host
                _         -> True
