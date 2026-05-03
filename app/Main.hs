module Main (main) where

import Control.Concurrent.STM
  ( atomically,
    modifyTVar,
    readTVar,
    writeTQueue,
  )
import Crawler.Scraper (urls)
import Crawler.State (initState)
import Crawler.Types (CrawlerState (visitedURLs), URL, urlQueue)
import Crawler.Types qualified as Crawler (Config)
import Crawler.Utils (checkRobots, extractDomain, makeManager, normalizeURL)
import Data.ByteString.Char8 qualified as BS
import Data.Default (def)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Conc (readTVarIO)
import Network.HTTP.Client qualified as HTTP
import Text.HTML.Scalpel (Config (manager), scrapeURLWithConfig)

crawl :: Crawler.Config -> URL -> IO (Set URL)
crawl cfg seedURL = do
  state <- initState cfg seedURL
  manager <- makeManager cfg

  processURL manager state seedURL

  readTVarIO (visitedURLs state)

processURL :: HTTP.Manager -> CrawlerState -> URL -> IO ()
processURL manager state url = do
  case extractDomain url of
    Nothing -> putStrLn $ "Invalid URL" <> show url
    Just baseURL -> do
      allowed <- checkRobots state baseURL url
      if allowed
        then scrapeAndEnqueue manager state url baseURL
        else putStrLn $ "Blocked by robots.txt" <> show url

scrapeAndEnqueue :: HTTP.Manager -> CrawlerState -> URL -> URL -> IO ()
scrapeAndEnqueue manager state url baseURL = do
  atomically $ modifyTVar (visitedURLs state) (Set.insert url)

  let scalpelCfg = def {manager = Just manager}
  foundURLs <- scrapeURLWithConfig scalpelCfg (BS.unpack url) urls
  case foundURLs of
    Nothing -> putStrLn "Failed to scrape"
    Just links -> do
      let normalized = map (normalizeURL baseURL) links
      atomically $ do
        visited <- readTVar (visitedURLs state)
        let unvisited = filter (`Set.notMember` visited) normalized
        mapM_ (writeTQueue (urlQueue state)) unvisited

main :: IO ()
main = do
  putStrLn "Hello"
