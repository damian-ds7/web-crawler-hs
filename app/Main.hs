module Main (main) where

import Control.Concurrent.STM
  ( atomically,
    modifyTVar,
    readTVar,
    writeTQueue,
  )
import Crawler.Scraper (urls)
import Crawler.Types (CrawlerState (visitedURLs), URL, urlQueue)
import Crawler.Utils (normalizeURL)
import Data.ByteString.Char8 qualified as BS
import Data.Default (def)
import Data.Set qualified as Set
import Network.HTTP.Client qualified as HTTP
import Text.HTML.Scalpel (Config (manager), scrapeURLWithConfig)

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
