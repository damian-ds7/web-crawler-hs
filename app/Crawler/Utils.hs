module Crawler.Utils where

import Crawler.Types (Config (userAgent), URL)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types.Header qualified as HTTP

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

normalizeURL :: URL -> URL -> URL
normalizeURL baseURL href
  | "/" `BS.isPrefixOf` href = baseURL <> BS.drop 1 href
  | otherwise = href
