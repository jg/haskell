-- | A Reddit comment aggregator
module Reddit where

import Reddit.Comment
import Reddit.Aggregator
import Reddit.Persist
import Reddit.Config
import Control.Monad.Reader

saveComments :: IO ()
saveComments = do
  Right config <- readConfig
  runReader (withConnection f) config
  where f conn = do
            comments <- getCommentPages 10
            mapM (persistComment conn) comments
            return ()
