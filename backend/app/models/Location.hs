{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Location where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Database.PostgreSQL.Simple (Connection, Only(..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data Location =
    Location { id :: Maybe Int
             , name :: String
             , approved :: Bool
             } deriving (Generic, Eq, Show)

instance ToJSON Location where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Location where

instance FromRow Location where
    fromRow = Location <$> field <*> field <*> field

create :: Connection -> Location -> IO Location
create conn location =
    let name' = name location
        approved' = approved location
    in do
        newLocations <- query conn "INSERT INTO locations (name, approved) VALUES (?, ?) RETURNING *" (name', approved')
        return $ head newLocations

find :: Connection -> Int -> IO (Maybe Location)
find conn id = do
    locations <- query conn "SELECT * FROM locations WHERE id = ?" $ Only id
    case locations of
        t:_ -> return $ Just t
        [] -> return Nothing

findAll :: Connection -> IO [Location]
findAll conn =
    query_ conn "SELECT * FROM locations WHERE approved = TRUE ORDER BY name"
