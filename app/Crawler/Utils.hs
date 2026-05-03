module Crawler.Utils
  ( makeManager,
    normalizeURL,
    extractDomain,
    checkRobots,
  )
where

import Crawler.Types (Config (userAgent), State, URL)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types.Header qualified as HTTP
import Network.URI
  ( URI (uriAuthority, uriScheme),
    URIAuth (uriRegName),
    parseURI,
  )

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
  | "/" `BS.isPrefixOf` href = baseURL <> href
  | otherwise = href

extractDomain :: URL -> Maybe URL
extractDomain url = do
  uri <- parseURI (BS.unpack url)
  auth <- uriAuthority uri
  let scheme = uriScheme uri
      domain = uriRegName auth
  return $ BS.pack $ scheme <> "//" <> domain

-- TODO: implement robots.txt parsing and appending to map in state
-- for each new domain with robots-txt
checkRobots :: State -> URL -> URL -> IO Bool
checkRobots _state _baseURL _url = return True
