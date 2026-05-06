module Crawler.Robots
  ( getRobots,
    checkRobots,
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
import Crawler.Types (Config (userAgent), State (config, robotsCache), URL)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Network.HTTP.Client (httpLbs, parseRequest, responseBody)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Robots (Robot, canAccess, parseRobots)

checkRobots :: Robot -> State -> URL -> Bool
checkRobots robot state = canAccess (userAgent (config state)) robot

getRobots :: HTTP.Manager -> State -> URL -> IO Robot
getRobots manager state baseURL = do
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
      robot <- fetchRobots manager baseURL
      atomically $ putTMVar slot robot
      return robot

fetchRobots :: HTTP.Manager -> URL -> IO Robot
fetchRobots manager baseURL = do
  let robotsURL = BS.unpack (baseURL <> "/robots.txt")
  request <- parseRequest robotsURL
  response <- httpLbs request manager
  let body = BL.toStrict $ responseBody response
  case parseRobots body of
    Left _err -> do
      putStrLn $ "Failed to fetch robots.txt for " <> show robotsURL
      pure ([], [])
    Right robot -> pure robot
