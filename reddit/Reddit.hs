-- | A Reddit comment aggregator
module Reddit where

import Reddit.Comment
import Reddit.Aggregator
import Reddit.Persist
import Reddit.Config
import Control.Monad.Reader

saveComments = do
  Right config <- liftIO readConfig
  runReaderT (withConnection saveComments') config
  where
    saveComments' conn = do
      comments <- getCommentPages 10
      mapM (persistComment conn) comments
      return ()
