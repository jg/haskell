{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}

-- | A Reddit comment aggregator
module Reddit where

import Network.Wreq
import Control.Lens
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Text.JSON.Generic
import Safe
import Database.PostgreSQL.Simple
import Data.Hash.MD5

newtype Url = Url String deriving (Show)

newtype User = User Text

newtype Hash = Hash String deriving (Show)

data Comment = Comment {
  title :: String,
  subreddit :: String,
  body :: String
  } deriving (Show, Typeable, Data)

userToUrl :: String -> Url
userToUrl u =
  Url $ "http://www.reddit.com/user/" ++ show u ++ "/comments/"

showUrl :: Url -> IO ()
showUrl (Url url) = putStrLn url

getPage :: Url -> IO (String)
getPage (Url url) = do
  result <- get url
  return $ T.unpack (TE.decodeUtf8 (BL.toStrict (result ^. responseBody)))

getUserPage :: IO String
getUserPage = getPage (Url "http://www.reddit.com/user/dons/comments/")

parseComments :: ArrowXml a => a XmlTree Comment
parseComments = css ".comment" >>>
                       proc x -> do
                         title_ <-         css ".title" //> getText -< x
                         subreddit_ <- css ".subreddit" //> getText -< x
                         body_ <-             css ".md" //> getText -< x
                         returnA -< Comment title_ subreddit_ body_

getNextPageUrl :: ArrowXml a => a XmlTree String
getNextPageUrl = css "a[rel~=next]" >>> getAttrValue "href"

getComments :: Url -> Int -> [Comment] -> IO [Comment]
getComments _ 0 comments = return (comments)
getComments url maxPages comments = do
  page <- getPage url
  document <- return (parseHtml page)
  pageComments <- runX $ parseComments <<< document
  hrefMaybe <- liftM headMay (runX (getNextPageUrl <<< document))
  case hrefMaybe of
    Just href -> do
      print href
      getComments (Url href) (maxPages-1) (comments ++ pageComments)
    Nothing -> return (comments)

getCommentPages :: Int -> IO [Comment]
getCommentPages pages = getComments (Url "http://www.reddit.com/user/dons") pages []

commentHash :: Comment -> Hash
commentHash (Comment title subreddit body) =
  Hash $ md5s (Str (title ++ subreddit ++ body))
foo = do
  comments <- getCommentPages 100
  writeFile "comments.json" (encodeJSON comments)

connectionUrl :: B.ByteString
connectionUrl = "host=localhost port=5432 dbname=reddit"
persistComment :: Comment -> IO ()
persistComment c@(Comment title subreddit body) = do
  conn <- connectPostgreSQL connectionUrl
  execute conn "INSERT INTO comments(title,subreddit,body,hash) VALUES (?, ?, ?, ?)"
    (title, subreddit, body, show (commentHash c))
  close conn
  return ()
