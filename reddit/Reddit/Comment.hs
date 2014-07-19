{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Reddit.Comment where

import Data.Time (UTCTime, getCurrentTime)
import Text.JSON.Generic
import Data.Maybe

data Comment = Comment {
  id :: Maybe Integer,
  title :: String,
  subreddit :: String,
  body :: String,
  hash :: Maybe String,
  created_at :: Maybe UTCTime,
  updated_at :: Maybe UTCTime
  } deriving (Show, Typeable, Data)

-- Simple Comment constructor, omits all of the db fluff
newComment :: String -> String -> String -> Comment
newComment title subreddit body =
  Comment Nothing title subreddit body Nothing Nothing Nothing
