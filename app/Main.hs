{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.HTML.Scalpel

normalizeURL :: String -> String -> String
normalizeURL baseURL ('/' : rest) = baseURL ++ "/" ++ rest
normalizeURL _ href = href

urls :: String -> Scraper String [URL]
urls baseURL = chroots "a" url
  where
    url :: Scraper String URL
    url = do
      href <- attr "href" "a"
      return $ normalizeURL baseURL href

scrapeLinks :: String -> String -> IO (Maybe [URL])
scrapeLinks baseURL searchURL = scrapeURL searchURL (urls baseURL)

main :: IO ()
main = do
  let baseURL = "https://webscraper.io"
      searchURL = "https://webscraper.io/test-sites/e-commerce/static/"
  result <- scrapeLinks baseURL searchURL
  case result of
    Nothing -> putStrLn "Scraping failed"
    Just links -> mapM_ putStrLn links
