module Crawler.Fetch
  ( makeManager,
    FetchError (..),
    fetchWithBlocking,
  )
where

import Control.Concurrent.STM
  ( atomically,
    modifyTVar,
    readTVar,
  )
import Control.Exception (try)
import Control.Monad (when)
import Crawler.Logger (LogLevel (..), logMessage)
import Crawler.Types (Config (userAgent), State (blockedDomains), URL)
import Crawler.Types qualified as Crawler
import Data.ByteString.Char8 (ByteString, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Set qualified as Set
import Network.HTTP.Client
  ( HttpException,
    Manager,
    httpLbs,
    managerModifyRequest,
    newManager,
    parseRequest,
    requestHeaders,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hUserAgent, status200, status300, statusCode)

makeManager :: Config -> IO Manager
makeManager cfg =
  newManager $
    tlsManagerSettings
      { managerModifyRequest = \req -> do
          req' <- managerModifyRequest tlsManagerSettings req
          return $
            req'
              { requestHeaders =
                  (hUserAgent, userAgent cfg) : requestHeaders req'
              }
      }

data FetchError
  = TransportError HttpException
  | HttpStatusError Int
  | DomainBlocked
  deriving (Show)

fetchURL :: Manager -> URL -> IO (Either FetchError ByteString)
fetchURL manager url = do
  result <- try $ do
    req <- parseRequest $ unpack url
    httpLbs req manager

  pure $ case result of
    Left e -> Left (TransportError e)
    Right res ->
      let code = responseStatus res
       in if code >= status200 && code < status300
            then Right (toStrict $ responseBody res)
            else Left (HttpStatusError $ statusCode code)

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

fetchWithBlocking :: Manager -> Crawler.State -> URL -> URL -> IO (Either FetchError ByteString)
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
