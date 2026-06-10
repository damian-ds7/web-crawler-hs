module Crawler.Robots
  ( cacheRobot,
    checkRobot,
    parseRobot,
  )
where

import Control.Concurrent.STM
  ( atomically,
    modifyTVar,
    newEmptyTMVar,
    putTMVar,
    readTVar,
  )
import Crawler.Types (Config (userAgent), State (config, robotsCache), URL)
import Data.ByteString.Char8 qualified as BS (ByteString, stripPrefix, takeWhile)
import Data.Map qualified as Map (insert, lookup)
import Network.HTTP.Robots (Robot, canAccess, parseRobots)

checkRobot :: Robot -> State -> URL -> URL -> Bool
checkRobot robot state baseURL url =
  let path = case BS.stripPrefix baseURL url of
        Just rest -> BS.takeWhile (/= '?') rest
        Nothing -> url
   in canAccess (userAgent (config state)) robot path

cacheRobot :: State -> URL -> Robot -> IO ()
cacheRobot state baseURL robot = do
  entry <- atomically $ do
    cache <- readTVar (robotsCache state)
    case Map.lookup baseURL cache of
      Just slot -> return (Right slot)
      Nothing -> do
        slot <- newEmptyTMVar
        modifyTVar (robotsCache state) (Map.insert baseURL slot)
        return (Left slot)

  case entry of
    Right _ -> pure ()
    Left slot -> do
      atomically $ putTMVar slot robot

parseRobot :: BS.ByteString -> Robot
parseRobot text =
  case parseRobots text of
    Left _err -> emptyRobot
    Right robot -> robot
  where
    emptyRobot = ([], [])
