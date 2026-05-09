module Main (main) where

import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    retry,
    tryReadTQueue,
    writeTQueue,
  )
import Control.Monad (forM_)
import Crawler.Logger (LogLevel (..), logMessage)
import Crawler.Robots (checkRobots, getRobots)
import Crawler.Scraper (urls)
import Crawler.State (initState)
import Crawler.Types (State (visitedURLs), URL, urlQueue)
import Crawler.Types qualified as Crawler
import Crawler.Utils (extractDomain, makeManager, normalizeURL)
import Data.ByteString.Char8 qualified as BS
import Data.Default (def)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Conc (readTVarIO)
import Network.HTTP.Client qualified as HTTP
import Text.HTML.Scalpel (Config (manager), scrapeURLWithConfig)

shouldStop :: Crawler.State -> Int -> Bool
shouldStop state depth =
  case Crawler.maxDepth (Crawler.config state) of
    Nothing -> False
    Just limit -> depth >= limit

crawl :: Crawler.Config -> IO (Set URL)
crawl cfg = do
  state <- initState cfg (Crawler.entrypoint cfg)
  manager <- makeManager cfg
  crawlLoop manager state
  readTVarIO (visitedURLs state)

crawlLoop :: HTTP.Manager -> Crawler.State -> IO ()
crawlLoop manager state = do
  inFlight <- newTVarIO (0 :: Int)
  let n = max 1 (Crawler.threadCount (Crawler.config state))
  replicateConcurrently_ n (workerLoop manager state inFlight)

workerLoop :: HTTP.Manager -> Crawler.State -> TVar Int -> IO ()
workerLoop manager state inFlight = do
  queueRecord <- atomically $ dequeueWork state inFlight
  case queueRecord of
    Just (url, depth) ->
      do
        atomically $ modifyTVar inFlight (+ 1)
        processURL manager state url depth
        atomically $ modifyTVar inFlight (\x -> x - 1)
        workerLoop manager state inFlight
    Nothing -> return ()

dequeueWork :: Crawler.State -> TVar Int -> STM (Maybe (URL, Int))
dequeueWork state inFlight = do
  queueRecord <- tryReadTQueue (urlQueue state)
  case queueRecord of
    Just (url, depth)
      | shouldStop state depth -> dequeueWork state inFlight
      | otherwise -> return $ Just (url, depth)
    Nothing -> do
      n <- readTVar inFlight
      if n /= 0 then retry else return Nothing

processURL :: HTTP.Manager -> Crawler.State -> URL -> Int -> IO ()
processURL manager state url depth = do
  case extractDomain url of
    Nothing -> logMessage Info $ "Invalid URL: " <> show url
    Just baseURL -> do
      robot <- getRobots manager state baseURL
      if checkRobots robot state baseURL url
        then scrapeAndEnqueue manager state url baseURL depth
        else logMessage Info $ "Blocked by robots.txt: " <> show url

scrapeAndEnqueue :: HTTP.Manager -> Crawler.State -> URL -> URL -> Int -> IO ()
scrapeAndEnqueue manager state url baseURL depth = do
  atomically $ modifyTVar (visitedURLs state) (Set.insert url)

  let scalpelCfg = def {manager = Just manager}
  foundURLs <- scrapeURLWithConfig scalpelCfg (BS.unpack url) urls
  case foundURLs of
    Nothing -> logMessage Error "Failed to scrape"
    Just links -> do
      let nonEmpty = filter (not . BS.null) links
          normalized = map (normalizeURL baseURL) nonEmpty
      atomically $ do
        visited <- readTVar (visitedURLs state)
        let unvisited = filter (`Set.notMember` visited) normalized
        forM_ unvisited $ writeTQueue (urlQueue state) . (,depth + 1)

main :: IO ()
main = do
  let cfg =
        Crawler.Config
          { Crawler.userAgent = "web-crawler-hs",
            Crawler.entrypoint = "https://webscraper.io/test-sites/",
            Crawler.threadCount = 8,
            Crawler.maxDepth = Just 2
          }
  result <- crawl cfg
  putStrLn "\nResults: "
  mapM_ BS.putStrLn (Set.toList result)
