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
import Control.Monad (forM_, when)
import Crawler.Fetch (FetchError (..), fetchURL, makeManager)
import Crawler.Logger (LogLevel (..), logMessage)
import Crawler.Parser (parseLinks)
import Crawler.Robots (cacheRobot, checkRobot, parseRobot)
import Crawler.State (initState)
import Crawler.Types (State (blockedDomains, visitedURLs), URL, urlQueue)
import Crawler.Types qualified as Crawler
import Crawler.Utils (extractDomain, normalizeURL)
import Data.ByteString.Char8 qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Conc (readTVarIO)
import Network.HTTP.Client qualified as HTTP

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

isDomainBlocked :: Crawler.State -> URL -> IO Bool
isDomainBlocked state baseURL = atomically $ do
  blocked <- readTVar (blockedDomains state)
  return $ Set.member baseURL blocked

blockDomain :: Crawler.State -> URL -> IO Bool
blockDomain state baseURL = atomically $ do
  blocked <- readTVar (blockedDomains state)
  if Set.member baseURL blocked
    then return False
    else do
      modifyTVar (blockedDomains state) (Set.insert baseURL)
      return True

fetchWithBlocking :: HTTP.Manager -> Crawler.State -> URL -> URL -> IO (Either FetchError BS.ByteString)
fetchWithBlocking manager state baseURL url = do
  blocked <- isDomainBlocked state baseURL
  if blocked
    then do
      logMessage Info $ "Skipping blocked domain: " <> show baseURL
      return $ Left DomainBlocked
    else do
      res <- fetchURL manager url
      case res of
        Left (HttpStatusError 429) -> do
          newlyBlocked <- blockDomain state baseURL
          when newlyBlocked $ logMessage Warn $ "Domain returned 429, blocking: " <> show baseURL
          return $ Left DomainBlocked
        _ -> return res

processURL :: HTTP.Manager -> Crawler.State -> URL -> Int -> IO ()
processURL manager state url depth = do
  case extractDomain url of
    Nothing -> logMessage Info $ "Skipping malformed URL: " <> show url
    Just baseURL -> handleRobotsTxt manager state url baseURL depth

handleRobotsTxt :: HTTP.Manager -> Crawler.State -> URL -> URL -> Int -> IO ()
handleRobotsTxt manager state url baseURL depth = do
  res <- fetchWithBlocking manager state baseURL (baseURL <> "/robots.txt")
  case res of
    Left DomainBlocked -> return ()
    Left err -> do
      logMessage Warn $ "Failed to fetch robots.txt, allowing all: " <> show baseURL <> " (" <> show err <> ")"
      scrapeAndEnqueue manager state url baseURL depth
    Right body -> do
      let robot = parseRobot body
      cacheRobot state baseURL robot
      if checkRobot robot state baseURL url
        then scrapeAndEnqueue manager state url baseURL depth
        else logMessage Info $ "Blocked by robots.txt: " <> show url

scrapeAndEnqueue :: HTTP.Manager -> Crawler.State -> URL -> URL -> Int -> IO ()
scrapeAndEnqueue manager state url baseURL depth = do
  atomically $ modifyTVar (visitedURLs state) (Set.insert url)
  res <- fetchWithBlocking manager state baseURL url
  case res of
    Left DomainBlocked -> return ()
    Left err -> logMessage Error $ "Failed to scrape " <> show url <> ": " <> show err
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
        Crawler.Config
          { Crawler.userAgent = "web-crawler-hs",
            Crawler.entrypoint = "https://webscraper.io/test-sites/",
            Crawler.threadCount = 8,
            Crawler.maxDepth = Just 2
          }
  result <- crawl cfg
  putStrLn "\nResults: "
  mapM_ BS.putStrLn (Set.toList result)
