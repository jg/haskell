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
import Control.Monad.Reader

import Reddit.Comment
import Reddit.Config

newtype Hash = Hash String

instance Show Hash where
  show (Hash s) = s

instance FromRow Comment where
  fromRow = Comment <$>
            field <*> field <*> field <*> field <*> field <*> field <*> field

commentHash :: Comment -> Hash
commentHash (Comment _ title subreddit body _ _ _) =
  Hash $ md5s (Str (title ++ subreddit ++ body))

getRandomComment = do
  withConnection (\conn -> do
  r <- query_ conn queryText :: IO [Comment]
  return (headMay r))
  where
    queryText = "select * from comments order by random() limit 1"

withConnection :: (Connection -> IO c) -> ReaderT Config IO c
withConnection f = do
  url <- connectionUrl
  liftIO $ bracket (connectPostgreSQL url) (close) f

isCommentInDb :: Connection -> Comment -> IO Bool
isCommentInDb conn c = do
  r <- query conn "select id from comments where hash = ?" (hash c) :: IO [(Only Int)]
  return $ (isJust . headMay) r
  where
    hash :: Comment -> Only String
    hash = Only . show . commentHash

persistComment :: Connection -> Comment -> IO ()
persistComment conn comment = do
  isInDb <- isCommentInDb conn comment
  if isInDb then return () else persistComment' conn comment

-- assumes comment isn't present in the database
persistComment' :: Connection -> Comment -> IO ()
persistComment' conn c@(Comment _ title subreddit body _ _ _) = do
  execute conn "INSERT INTO comments(title,subreddit,body,hash) VALUES (?, ?, ?, ?)"
    (title, subreddit, body, show (commentHash c))
  return ()
