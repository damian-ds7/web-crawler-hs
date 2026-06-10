module Crawler.Robots
  ( getRobot,
    checkRobot,
  )
where

import Control.Concurrent.STM
  ( atomically,
    modifyTVar,
    newEmptyTMVar,
    putTMVar,
    readTMVar,
    readTVar,
  )
import Crawler.Fetch (FetchError (DomainBlocked), fetchWithBlocking)
import Crawler.Logger (LogLevel (Warn), logMessage)
import Crawler.Types (Config (userAgent), State (config, robotsCache), URL)
import Data.ByteString.Char8 qualified as BS (ByteString, stripPrefix, takeWhile)
import Data.Either (fromRight)
import Data.Map qualified as Map (insert, lookup)
import Network.HTTP.Client (Manager)
import Network.HTTP.Robots (Robot, canAccess, parseRobots)

emptyRobot :: Robot
emptyRobot = ([], [])

checkRobot :: Robot -> State -> URL -> URL -> Bool
checkRobot robot state baseURL url =
  let path = case BS.stripPrefix baseURL url of
        Just rest -> BS.takeWhile (/= '?') rest
        Nothing -> url
   in canAccess (userAgent (config state)) robot path

getRobot :: Manager -> State -> URL -> IO Robot
getRobot manager state baseURL = do
  entry <- atomically $ do
    cache <- readTVar (robotsCache state)
    case Map.lookup baseURL cache of
      Just slot -> return (Right slot)
      Nothing -> do
        slot <- newEmptyTMVar
        modifyTVar (robotsCache state) (Map.insert baseURL slot)
        return (Left slot)

  case entry of
    Right slot -> atomically $ readTMVar slot
    Left slot -> do
      robot <- fetchRobot manager state baseURL
      atomically $ putTMVar slot robot
      return robot

fetchRobot :: Manager -> State -> URL -> IO Robot
fetchRobot manager state baseURL = do
  res <- fetchWithBlocking manager state baseURL (baseURL <> "/robots.txt")
  case res of
    Left DomainBlocked -> pure emptyRobot
    Left err -> do
      logMessage Warn $ "Failed to fetch robots.txt, allowing all: " <> show baseURL <> " (" <> show err <> ")"
      pure emptyRobot
    Right body -> pure $ parseRobot body

parseRobot :: BS.ByteString -> Robot
parseRobot =
  fromRight emptyRobot . parseRobots
