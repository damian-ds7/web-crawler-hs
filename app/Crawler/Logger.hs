module Crawler.Logger
  ( logMessage,
    LogLevel (..),
  )
where

import Data.Time

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show, Eq)

logMessage :: LogLevel -> String -> IO ()
logMessage level message = do
  now <- getCurrentTime
  putStrLn (show now ++ " | " ++ show level ++ " | " ++ message)
