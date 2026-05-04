module Crawler.Types
  ( Config (..),
    State (..),
    URL,
    Domain (..),
  )
where

import Control.Concurrent.STM (TMVar, TQueue)
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Conc (TVar)
import Network.HTTP.Robots (Robot)

type URL = ByteString

newtype Domain = Domain String deriving (Show, Eq, Ord)

data Config = Config
  { userAgent :: ByteString,
    threadCount :: Int,
    maxDepth :: Maybe Int
  }

data State = State
  { visitedURLs :: TVar (Set URL),
    urlQueue :: TQueue (URL, Int),
    robotsCache :: TVar (Map Domain (TMVar Robot)),
    config :: Config
  }
