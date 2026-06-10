module Main (main) where

import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    retry,
    tryReadTQueue,
    writeTQueue,
  )
import Control.Monad (forM_)
import Crawler.Fetch (FetchError (..), fetchWithBlocking, makeManager)
import Crawler.Logger (LogLevel (..), logMessage)
import Crawler.Parser (parseLinks)
import Crawler.Robots (checkRobot, getRobot)
import Crawler.State (initState)
import Crawler.Types (Config (Config, entrypoint, maxDepth, threadCount, userAgent), State (config, visitedURLs), URL, urlQueue)
import Crawler.Utils (extractDomain, normalizeURL)
import Data.ByteString.Char8 qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Conc (readTVarIO)
import Network.HTTP.Client (Manager)

shouldStop :: State -> Int -> Bool
shouldStop state depth =
  case maxDepth (config state) of
    Nothing -> False
    Just limit -> depth >= limit

crawl :: Config -> IO (Set URL)
crawl cfg = do
  state <- initState cfg (entrypoint cfg)
  manager <- makeManager cfg
  crawlLoop manager state
  readTVarIO (visitedURLs state)

crawlLoop :: Manager -> State -> IO ()
crawlLoop manager state = do
  inFlight <- newTVarIO (0 :: Int)
  let n = max 1 (threadCount (config state))
  replicateConcurrently_ n (workerLoop manager state inFlight)

workerLoop :: Manager -> State -> TVar Int -> IO ()
workerLoop manager state inFlight = do
  queueRecord <- atomically $ do
    m <- tryReadTQueue (urlQueue state)
    case m of
      Just work -> return $ Just work
      Nothing -> do
        n <- readTVar inFlight
        if n /= 0 then retry else return Nothing

  case queueRecord of
    Just (url, depth)
      | shouldStop state depth -> workerLoop manager state inFlight
      | otherwise -> do
          atomically $ modifyTVar inFlight (+ 1)
          processURL manager state url depth
          atomically $ modifyTVar inFlight (\x -> x - 1)
          workerLoop manager state inFlight
    Nothing -> return ()

processURL :: Manager -> State -> URL -> Int -> IO ()
processURL manager state url depth = do
  case extractDomain url of
    Nothing -> logMessage Info $ "Skipping malformed URL: " <> show url
    Just baseURL -> do
      robot <- getRobot manager state baseURL
      if checkRobot robot state baseURL url
        then scrapeAndEnqueue manager state url baseURL depth
        else logMessage Info $ "Blocked by robots.txt: " <> show url

scrapeAndEnqueue :: Manager -> State -> URL -> URL -> Int -> IO ()
scrapeAndEnqueue manager state url baseURL depth = do
  atomically $ modifyTVar (visitedURLs state) (Set.insert url)
  res <- fetchWithBlocking manager state baseURL url
  case res of
    Left DomainBlocked -> return ()
    Left err -> logMessage Warn $ "Failed to scrape " <> show url <> ": " <> show err
    Right body -> do
      let foundURLs = parseLinks body
          nonEmpty = filter (not . BS.null) foundURLs
          normalized = map (normalizeURL baseURL) nonEmpty
      atomically $ do
        visited <- readTVar (visitedURLs state)
        let unvisited = filter (`Set.notMember` visited) normalized
        forM_ unvisited $ writeTQueue (urlQueue state) . (,depth + 1)

main :: IO ()
main = do
  let cfg =
        Config
          { userAgent = "web-crawler-hs",
            entrypoint = "https://webscraper.io/test-sites/",
            threadCount = 8,
            maxDepth = Just 3
          }
  result <- crawl cfg
  putStrLn "\nResults: "
  mapM_ BS.putStrLn (Set.toList result)
