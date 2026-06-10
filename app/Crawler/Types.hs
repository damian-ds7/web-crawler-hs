module Crawler.Types
  ( Config (..),
    State (..),
    URL,
  )
where

import Control.Concurrent.STM (TMVar, TQueue)
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Conc (TVar)
import Network.HTTP.Robots (Robot)

type URL = ByteString

data Config = Config
  { userAgent :: ByteString,
    entrypoint :: URL,
    threadCount :: Int,
    maxDepth :: Maybe Int
  }

data State = State
  { visitedURLs :: TVar (Set URL),
    urlQueue :: TQueue (URL, Int),
    robotsCache :: TVar (Map URL (TMVar Robot)),
    blockedDomains :: TVar (Set URL),
    config :: Config
  }
