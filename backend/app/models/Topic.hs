{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Topic where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Database.PostgreSQL.Simple (Connection, Only(..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data Topic =
    Topic { id :: Maybe Int
          , name :: String
          , approved :: Bool
          } deriving (Generic, Eq, Show)

instance ToJSON Topic where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Topic where

instance FromRow Topic where
    fromRow = Topic <$> field <*> field <*> field

create :: Connection -> Topic -> IO Topic
create conn topic =
    let name' = name topic
        approved' = approved topic
    in do
        newTopics <- query conn "INSERT INTO topics (name, approved) VALUES (?, ?) RETURNING *" (name', approved')
        return $ head newTopics

find :: Connection -> Int -> IO (Maybe Topic)
find conn id = do
    topics <- query conn "SELECT * FROM topics WHERE id = ?" $ Only id
    case topics of
        t:_ -> return $ Just t
        [] -> return Nothing

findAll :: Connection -> IO [Topic]
findAll conn =
    query_ conn "SELECT * FROM topics WHERE approved = TRUE ORDER BY name"
