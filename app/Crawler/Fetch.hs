module Crawler.Fetch
  ( makeManager,
    fetchURL,
  )
where

import Control.Exception (try)
import Crawler.Types (Config (userAgent), URL)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types qualified as HTTP

makeManager :: Config -> IO HTTP.Manager
makeManager cfg =
  HTTP.newManager $
    HTTP.tlsManagerSettings
      { HTTP.managerModifyRequest = \req -> do
          req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
          return $
            req'
              { HTTP.requestHeaders =
                  (HTTP.hUserAgent, userAgent cfg) : HTTP.requestHeaders req'
              }
      }

data FetchError
  = TransportError HTTP.HttpException
  | HttpStatusError Int
  deriving (Show)

fetchURL :: HTTP.Manager -> URL -> IO (Either FetchError BS.ByteString)
fetchURL manager url = do
  result <- try $ do
    req <- HTTP.parseRequest $ BS.unpack url
    HTTP.httpLbs req manager

  pure $ case result of
    Left e -> Left (TransportError e)
    Right res ->
      let code = HTTP.statusCode (HTTP.responseStatus res)
       in if code >= 200 && code < 300
            then Right (BL.toStrict $ HTTP.responseBody res)
            else Left (HttpStatusError code)
