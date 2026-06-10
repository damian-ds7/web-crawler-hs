module Crawler.Fetch
  ( makeManager,
    fetchURL,
    FetchError (..),
  )
where

import Control.Exception (try)
import Crawler.Types (Config (userAgent), URL)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.ByteString.Lazy (toStrict)
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
