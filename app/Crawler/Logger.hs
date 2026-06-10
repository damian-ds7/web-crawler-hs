module Crawler.Logger
  ( logMessage,
    LogLevel (..),
  )
where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Text.Printf (printf)

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show, Eq)

logMessage :: LogLevel -> String -> IO ()
logMessage level message = do
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%3Q" now -- fixed: .NNN ms
  printf "%s | %-5s | %s\n" ts (show level) message
