{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Prelude
import qualified Prelude as P
import Shelly
import Data.Text as T
import System.Environment
import Text.Read
import Control.Applicative
import Data.Maybe

newtype IssueNumber = IssueNumber Integer

instance Show IssueNumber where
  show (IssueNumber n) = show n

data Commit = Commit {
  sha :: Text,
  description :: Text
  }

instance Show Commit where
  show (Commit sha description) = unpack description

gitCommand:: IssueNumber -> Shelly.FilePath
gitCommand issueNumber =
  fromText $ pack ("git log --format=oneline --grep " ++ "\"Refs #" ++ show issueNumber ++ "\"")

help :: String
help = "Example Usage: ./issueDiff 8567"

displayHelp :: IO ()
displayHelp = putStrLn(help)

parseIssueNumber :: [String] -> Maybe IssueNumber
parseIssueNumber (issueNumber:_) = case readMaybe issueNumber of
  Nothing -> Nothing
  Just number -> Just (IssueNumber number)

parseGitLogLine :: Text -> Commit
parseGitLogLine line = Commit (P.head parts) (T.unwords $ P.tail parts)
  where parts = split (==' ') line

issueCommits :: IssueNumber -> Sh [Commit]
issueCommits issue = shelly $ silently $ escaping False $ do
    results <- T.lines <$> run (gitCommand issue) []
    return $ fmap parseGitLogLine results

displayCommitDiff :: Commit -> Sh ()
displayCommitDiff (Commit sha _) = shelly $ print_commands True $ escaping False $ do
  run (fromText (T.append (T.append "git -c color.status=always show " sha) " | less -r")) []
  return ()


fromString :: String -> Shelly.FilePath
fromString = fromText . pack

main :: IO ()
main = do
  args <- getArgs
  case parseIssueNumber args of
    Nothing -> return ()
    Just issueNumber ->
      shelly $ do
        commits <- issueCommits issueNumber
        displayCommitDiff (commits !! 1)
