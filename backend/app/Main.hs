{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (loadFile)
import Data.Default (def)
import Network.Wai.Middleware.RequestLogger (autoFlush, outputFormat, mkRequestLogger, OutputFormat( Apache ), IPAddrSource( FromHeader ))
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment (getEnv)
import Web.Scotty

getPort :: IO Int
getPort = fmap read (getEnv "PORT")

main :: IO ()
main = do
    loadFile False "config/config.env"
    port <- getPort
    requestLogger <- mkRequestLogger def { outputFormat = Apache FromHeader, autoFlush = False }
    scotty port $ do
        middleware requestLogger
        middleware $ staticPolicy (addBase "dist")

        get "/" $ do
            setHeader "Content-Type" "text/html"
            file "dist/index.html"
