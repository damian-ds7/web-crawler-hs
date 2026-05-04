module Crawler.Scraper
  ( urls,
  )
where

import Crawler.Types (URL)
import Data.ByteString.Char8 as BS (pack)
import Text.HTML.Scalpel (Scraper, attr, chroots)

urls :: Scraper String [URL]
urls = chroots "a" url
  where
    url :: Scraper String URL
    url = BS.pack <$> attr "href" "a"
