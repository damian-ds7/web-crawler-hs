module Crawler.Scraper
  ( urls,
  )
where

import Crawler.Fetch (fetchURL)
import Crawler.Types (URL)
import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Client (Manager)
import Text.Regex.TDFA ((=~))

urls :: Manager -> URL -> IO [URL]
urls manager url = do
  res <- fetchURL manager url
  case res of
    Left _ -> return []
    Right body -> do
      -- Regex to match <a ... href="url" ...> or <a ... href='url' ...>
      let regex = "<a[^>]+href=[\"']([^\"']+)[\"']" :: BS.ByteString
      let matches = body =~ regex :: [[BS.ByteString]]
      return [m !! 1 | m <- matches, length m > 1]
