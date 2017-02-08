{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Conference where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Time.Calendar (Day, fromGregorian)
import Database.PostgreSQL.Simple (Connection, Only(..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data Conference =
    Conference { id :: Maybe Int
               , name :: String
               , url :: String
               , startDate :: Day
               , endDate :: Day
               , cfpStartDate :: Maybe Day
               , cfpEndDate :: Maybe Day
               , approved :: Bool
               } deriving (Generic, Eq, Show)

instance ToJSON Conference where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Conference where

instance FromRow Conference where
    fromRow = Conference <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

create :: Connection -> Conference -> IO Conference
create conn conference =
    let name' = name conference
        url' = url conference
        startDate' = startDate conference
        endDate' = endDate conference
        cfpStartDate' = cfpStartDate conference
        cfpEndDate' = cfpEndDate conference
        approved' = approved conference
    in do
        newConfs <- query conn "INSERT INTO conferences (name, url, start_date, end_date, cfp_start_date, cfp_end_date, approved) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING *" (name', url', startDate', endDate', cfpStartDate', cfpEndDate', approved')
        return $ head newConfs

find :: Connection -> Int -> IO (Maybe Conference)
find conn id = do
    confs <- query conn "SELECT * FROM conferences WHERE id = ?" $ Only id
    case confs of
        c:_ -> return $ Just c
        [] -> return Nothing

findAll :: Connection -> IO [Conference]
findAll conn =
    query_ conn "SELECT * FROM conferences WHERE approved = TRUE"
