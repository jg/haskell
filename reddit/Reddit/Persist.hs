{-# LANGUAGE OverloadedStrings #-}
module Reddit.Persist where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Data.Hash.MD5
import Data.Maybe
import Control.Applicative
import Control.Exception.Base
import qualified Data.ByteString as B
import Safe

import Reddit.Comment

newtype Hash = Hash String

instance Show Hash where
  show (Hash s) = s

instance FromRow Comment where
  fromRow = Comment <$>
            field <*> field <*> field <*> field <*> field <*> field <*> field


commentHash :: Comment -> Hash
commentHash (Comment _ title subreddit body _ _ _) =
  Hash $ md5s (Str (title ++ subreddit ++ body))

connectionUrl :: B.ByteString
connectionUrl = "host=localhost port=5432 dbname=reddit password=testtest"

getRandomComment :: IO (Maybe Comment)
getRandomComment = do
  withConnection (\conn -> do
  r <- query_ conn queryText :: IO [Comment]
  return (headMay r))
  where
    queryText = "select * from comments order by random() limit 1"

withConnection :: (Connection -> IO c) -> IO c
withConnection f = bracket (connectPostgreSQL connectionUrl) (close) f

isCommentInDb :: Connection -> Comment -> IO Bool
isCommentInDb conn c = do
  r <- query conn "select id from comments where hash = ?" (hash c) :: IO [(Only Int)]
  return $ (isJust . headMay) r
  where
    hash :: Comment -> Only String
    hash = Only . show . commentHash

persistComment :: Connection -> Comment -> IO ()
persistComment conn c@(Comment _ title subreddit body _ _ _) = do
  execute conn "INSERT INTO comments(title,subreddit,body,hash) VALUES (?, ?, ?, ?)"
    (title, subreddit, body, show (commentHash c))
  return ()
