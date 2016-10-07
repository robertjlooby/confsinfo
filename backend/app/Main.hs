{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "dist")

    get "/" $ do
        setHeader "Content-Type" "text/html"
        file "dist/index.html"

    get "/health" $ do
        text "OK"
