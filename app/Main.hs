module Main (main) where

import Control.Concurrent.STM
  ( atomically,
    modifyTVar,
    readTVar,
    tryReadTQueue,
    writeTQueue,
  )
import Control.Monad (forM_, unless)
import Crawler.Scraper (urls)
import Crawler.State (initState)
import Crawler.Types (State (visitedURLs), URL, urlQueue)
import Crawler.Types qualified as Crawler
import Crawler.Utils (checkRobots, extractDomain, makeManager, normalizeURL)
import Data.ByteString.Char8 qualified as BS
import Data.Default (def)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Conc (readTVarIO)
import Network.HTTP.Client qualified as HTTP
import Text.HTML.Scalpel (Config (manager), scrapeURLWithConfig)

-- TODO: add error handling when requests are blocked etc.
-- mutlithreading, proper robots.txt parsing and usage

shouldStop :: Crawler.State -> Int -> Bool
shouldStop state depth =
  case Crawler.maxDepth (Crawler.config state) of
    Nothing -> False
    Just limit -> depth >= limit

crawl :: Crawler.Config -> URL -> IO (Set URL)
crawl cfg seedURL = do
  state <- initState cfg seedURL
  manager <- makeManager cfg

  crawlLoop manager state

  readTVarIO (visitedURLs state)

-- TODO: will probably be done wtih a thread pool in the future
crawlLoop :: HTTP.Manager -> Crawler.State -> IO ()
crawlLoop manager state = do
  mUrl <- atomically $ tryReadTQueue (urlQueue state)
  case mUrl of
    Nothing -> return ()
    Just (url, depth) -> do
      unless (shouldStop state depth) $ do
        processURL manager state url depth
      crawlLoop manager state

processURL :: HTTP.Manager -> Crawler.State -> URL -> Int -> IO ()
processURL manager state url depth = do
  case extractDomain url of
    Nothing -> putStrLn $ "Invalid URL" <> show url
    Just baseURL -> do
      allowed <- checkRobots state baseURL url
      if allowed
        then scrapeAndEnqueue manager state url baseURL depth
        else putStrLn $ "Blocked by robots.txt" <> show url

scrapeAndEnqueue :: HTTP.Manager -> Crawler.State -> URL -> URL -> Int -> IO ()
scrapeAndEnqueue manager state url baseURL depth = do
  atomically $ modifyTVar (visitedURLs state) (Set.insert url)

  let scalpelCfg = def {manager = Just manager}
  foundURLs <- scrapeURLWithConfig scalpelCfg (BS.unpack url) urls
  case foundURLs of
    Nothing -> putStrLn "Failed to scrape"
    Just links -> do
      let nonEmpty = filter (not . BS.null) links
          normalized = map (normalizeURL baseURL) nonEmpty
      atomically $ do
        visited <- readTVar (visitedURLs state)
        let unvisited = filter (`Set.notMember` visited) normalized
        forM_ unvisited $ writeTQueue (urlQueue state) . (,depth + 1)

main :: IO ()
main = do
  let cfg = Crawler.Config {Crawler.userAgent = "web-crawler-hs", Crawler.threadCount = 1, Crawler.maxDepth = Just 2}
  result <- crawl cfg "https://webscraper.io/test-sites/e-commerce/static/"
  mapM_ BS.putStrLn (Set.toList result)
