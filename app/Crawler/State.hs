module Crawler.State
  ( initState,
  )
where

import Control.Concurrent.STM (atomically, newTQueueIO, newTVarIO, writeTQueue)
import Crawler.Types (Config, State (..), URL)
import Data.Map qualified as Map (empty)
import Data.Set qualified as Set (empty)

initState :: Config -> URL -> IO State
initState cfg seedURL = do
  visited <- newTVarIO Set.empty
  queue <- newTQueueIO
  robots <- newTVarIO Map.empty
  blocked <- newTVarIO Set.empty
  atomically $ writeTQueue queue (seedURL, 0)
  return $
    State
      { visitedURLs = visited,
        urlQueue = queue,
        robotsCache = robots,
        blockedDomains = blocked,
        config = cfg
      }
