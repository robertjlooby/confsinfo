{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (loadFile)
import Conference (create, findAll)
import Data.Default (def)
import Network.Wai.Middleware.RequestLogger (autoFlush, outputFormat, mkRequestLogger, OutputFormat( Apache ), IPAddrSource( FromHeader ))
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
    requestLogger <- mkRequestLogger def { autoFlush = True }
    scotty port $ do
        middleware requestLogger
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

        get "/.well-known/acme-challenge/amx9BslVaZ7LPlb9E7xP4rhptIUf0bsVPY5csgdPqcc" $
            text "amx9BslVaZ7LPlb9E7xP4rhptIUf0bsVPY5csgdPqcc.qJyOa5Z0-NzM8PdXclBdszGOX1_pxakBPsuLc4_efbI"
