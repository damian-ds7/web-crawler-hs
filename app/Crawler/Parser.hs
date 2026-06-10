module Crawler.Parser
  ( parseLinks,
  )
where

import Crawler.Types (URL)
import Data.ByteString.Char8 qualified as BS
import Text.Regex.TDFA ((=~))

parseLinks :: BS.ByteString -> [URL]
parseLinks body =
  -- Regex to match <a ... href="url" ...> or <a ... href='url' ...>
  let regex = "<a[^>]+href=[\"']([^\"']+)[\"']" :: BS.ByteString
      matches = body =~ regex :: [[BS.ByteString]]
   in [m !! 1 | m <- matches, length m > 1]
