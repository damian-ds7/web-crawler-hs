module Crawler.Types where

import Control.Concurrent.STM (TMVar, TQueue)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Conc (TVar)
import Network.HTTP.Robots (Robot)

type URL = ByteString

newtype Domain = Domain String deriving (Show, Eq, Ord)

data Config = Config
  { userAgent :: ByteString,
    threadCount :: Int
  }

data CrawlerState = CrawlerState
  { visitedURLs :: TVar (Set URL),
    urlQueue :: TQueue URL,
    robotsCache :: TVar (Map Domain (TMVar Robot)),
    config :: Config
  }
