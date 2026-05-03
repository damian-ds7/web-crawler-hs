module Crawler.State where

import Control.Concurrent.STM (newTQueueIO, newTVarIO, writeTQueue)
import Crawler.Types (Config, CrawlerState (..), URL)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Conc (atomically)

initState :: Config -> URL -> IO CrawlerState
initState cfg seedURL = do
  visited <- newTVarIO Set.empty
  queue <- newTQueueIO
  robots <- newTVarIO Map.empty
  atomically $ writeTQueue queue seedURL
  return $
    CrawlerState
      { visitedURLs = visited,
        urlQueue = queue,
        robotsCache = robots,
        config = cfg
      }
