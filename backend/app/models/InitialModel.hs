{-# LANGUAGE DeriveGeneric #-}

module InitialModel where

import qualified Audience
import qualified Conference
import qualified Language
import qualified Location
import qualified Topic
import GHC.Generics
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Database.PostgreSQL.Simple (Connection, Only(..), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

data InitialModel =
    InitialModel { audiences :: [String]
                 , conferences :: [Conference.Conference]
                 , includePastEvents :: Bool
                 , languages :: [String]
                 , locations :: [String]
                 , topics :: [String]
                 } deriving (Generic, Eq, Show)

instance ToJSON InitialModel where
    toEncoding = genericToEncoding defaultOptions

find :: Connection -> IO InitialModel
find conn = do
    audiences <- fmap Audience.name <$> Audience.findAll conn
    --conferences <- Conference.findAll conn
    languages <- fmap Language.name <$> Language.findAll conn
    locations <- fmap Location.name <$> Location.findAll conn
    topics <- fmap Topic.name <$> Topic.findAll conn
    return $ InitialModel audiences [] False languages locations topics
