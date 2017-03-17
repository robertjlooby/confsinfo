{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Audience where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Database.PostgreSQL.Simple (Connection, Only(..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data Audience =
    Audience { id :: Maybe Int
             , name :: String
             , approved :: Bool
             } deriving (Generic, Eq, Show)

instance ToJSON Audience where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Audience where

instance FromRow Audience where
    fromRow = Audience <$> field <*> field <*> field

create :: Connection -> Audience -> IO Audience
create conn audience =
    let name' = name audience
        approved' = approved audience
    in do
        newAudiences <- query conn "INSERT INTO audiences (name, approved) VALUES (?, ?) RETURNING *" (name', approved')
        return $ head newAudiences

find :: Connection -> Int -> IO (Maybe Audience)
find conn id = do
    audiences <- query conn "SELECT * FROM audiences WHERE id = ?" $ Only id
    case audiences of
        t:_ -> return $ Just t
        [] -> return Nothing

findAll :: Connection -> IO [Audience]
findAll conn =
    query_ conn "SELECT * FROM audiences WHERE approved = TRUE ORDER BY name"
