{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Network.Wai.Middleware.RequestLogger (autoFlush, outputFormat, mkRequestLogger, OutputFormat( Apache ), IPAddrSource( FromHeader ))
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import RunAll (migrateAll)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import Web.Scotty

main :: IO ()
main = do
    migrateAll $ pack $ unsafePerformIO $ getEnv "DATABASE_URL"
    scotty (read $ unsafePerformIO $ getEnv "PORT") $ do
      middleware $ unsafePerformIO $ mkRequestLogger def { outputFormat = Apache FromHeader, autoFlush = False }
      middleware $ staticPolicy (addBase "dist")

      get "/" $ do
          setHeader "Content-Type" "text/html"
          file "dist/index.html"

      get "/health" $ do
          text "OK"
