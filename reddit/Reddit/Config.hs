module Reddit.Config
       (
         Config,
         connectionUrl,
         readConfig
       ) where

import qualified Data.Map as Map
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe

import Reddit.Config.Parser

newtype Config = Config (Map.Map String String)

readConfig :: IO (Config)
readConfig = do
  result <- readConfigFile "config.cfg"
  let Right map = result in return (Config map)

connectionUrl :: ReaderT Config IO B.ByteString
connectionUrl = do
  config <- ask
  return $ BC.pack (url config)
  where
    names = ["host", "port", "dbname", "password"]
    values config = fmap (lookupVar config) names
    url config = unwords $
      zipWith (\a b -> a ++ "=" ++ b) names (values config)

lookupVar :: Config -> String -> String
lookupVar config name = fromJust (Map.lookup name (f config))
                        where f (Config c) = c
