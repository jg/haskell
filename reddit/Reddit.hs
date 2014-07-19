-- | A Reddit comment aggregator
module Reddit where

import Reddit.Comment
import Reddit.Aggregator
import Reddit.Persist

foo :: IO ()
foo = do
  withConnection (\conn -> do
  comments <- getCommentPages 1
  mapM (persistComment conn) comments
  return ()
  )
