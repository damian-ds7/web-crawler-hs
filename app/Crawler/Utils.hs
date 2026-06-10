module Crawler.Utils
  ( normalizeURL,
    extractDomain,
  )
where

import Crawler.Types (URL)
import Data.ByteString.Char8 qualified as BS
import Network.URI
  ( URI (uriAuthority, uriScheme),
    URIAuth (uriRegName),
    parseURI,
  )

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
