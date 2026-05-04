module Crawler.State
  ( initState,
  )
where

import Control.Concurrent.STM (newTQueueIO, newTVarIO, writeTQueue)
import Crawler.Types (Config, State (..), URL)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Conc (atomically)

initState :: Config -> URL -> IO State
initState cfg seedURL = do
  visited <- newTVarIO Set.empty
  queue <- newTQueueIO
  robots <- newTVarIO Map.empty
  atomically $ writeTQueue queue (seedURL, 0)
  return $
    State
      { visitedURLs = visited,
        urlQueue = queue,
        robotsCache = robots,
        config = cfg
      }
