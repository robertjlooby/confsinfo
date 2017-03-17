{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Language where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Database.PostgreSQL.Simple (Connection, Only(..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data Language =
    Language { id :: Maybe Int
             , name :: String
             , approved :: Bool
             } deriving (Generic, Eq, Show)

instance ToJSON Language where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Language where

instance FromRow Language where
    fromRow = Language <$> field <*> field <*> field

create :: Connection -> Language -> IO Language
create conn language =
    let name' = name language
        approved' = approved language
    in do
        newLanguages <- query conn "INSERT INTO languages (name, approved) VALUES (?, ?) RETURNING *" (name', approved')
        return $ head newLanguages

find :: Connection -> Int -> IO (Maybe Language)
find conn id = do
    languages <- query conn "SELECT * FROM languages WHERE id = ?" $ Only id
    case languages of
        t:_ -> return $ Just t
        [] -> return Nothing

findAll :: Connection -> IO [Language]
findAll conn =
    query_ conn "SELECT * FROM languages WHERE approved = TRUE ORDER BY name"
