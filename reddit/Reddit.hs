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
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Data.Hash.MD5
import Data.Maybe
import Control.Applicative
import Data.Time.Clock

newtype Url = Url String deriving (Show)

newtype User = User Text

newtype Hash = Hash String

instance Show Hash where
  show (Hash s) = s

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

instance FromRow Comment where
  fromRow = Comment <$>
            field <*> field <*> field <*> field <*> field <*> field <*> field

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
    Nothing -> do
      print "nothing!"
      return (comments)

getCommentPages :: Int -> IO [Comment]
getCommentPages pages = getComments (Url "http://www.reddit.com/user/dons") pages []

commentHash :: Comment -> Hash
commentHash (Comment _ title subreddit body _ _ _) =
  Hash $ md5s (Str (title ++ subreddit ++ body))

foo :: IO ()
foo = do
  conn <- connectPostgreSQL connectionUrl
  comments <- getCommentPages 1
  mapM (persistComment conn) comments
  close conn
  return ()

connectionUrl :: B.ByteString
connectionUrl = "host=localhost port=5432 dbname=reddit password=testtest"

getRandomComment :: IO (Maybe Comment)
getRandomComment = do
  conn <- connectPostgreSQL connectionUrl
  r <- query_ conn queryText :: IO [Comment]
  close conn
  return (headMay r)
  where
    queryText = "select * from comments order by random() limit 1"

bar = do
  conn <- connectPostgreSQL connectionUrl
  commentMay <- getRandomComment
  close conn
  isCommentInDb conn (fromJust commentMay)

isCommentInDb :: Connection -> Comment -> IO Bool
isCommentInDb conn c = do
  r <- query conn "select id from comments where hash = ?" (hash c) :: IO [(Only Int)]
  print (fromOnly (hash c))
  return $ (isJust . headMay) r
  where
    hash :: Comment -> Only String
    hash = Only . show . commentHash

persistComment :: Connection -> Comment -> IO ()
persistComment conn c@(Comment _ title subreddit body _ _ _) = do
  execute conn "INSERT INTO comments(title,subreddit,body,hash) VALUES (?, ?, ?, ?)"
    (title, subreddit, body, show (commentHash c))
  return ()
