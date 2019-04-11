{-# LANGUAGE CPP #-}

{-|
A library for listing "files" in an http "directory".

@
import Network.HTTP.Directory
import qualified Data.Text as T

main = do
  files <- httpDirectory "https://example.com/some/dir/"
  mapM_ T.putStrLn files
  httpFileSize (head files) >>= print
@
-}

module Network.HTTP.Directory
       ( httpDirectory,
         httpGlob,
         httpFileSize,
         httpRedirect,
         httpRedirects
       ) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Text (Text)

import Network.HTTP.Client (hrRedirects, httpLbs, httpNoBody, Manager, method,
                            parseRequest, responseBody, responseHeaders,
                            responseOpenHistory, responseStatus)
import Network.HTTP.Types (hContentLength, statusCode)

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

-- | list the file links (hrefs) in an http directory
--
-- Note if the directory (webpage) url is redirected you may need to use
-- 'httpRedirect' to determine the actual final url prefix for relative links
-- (files).
httpDirectory :: Manager -> String -> IO [Text]
httpDirectory mgr url = do
  request <- parseRequest url
  response <- httpLbs request mgr
  if statusCode (responseStatus response) /= 200
    then error $ show $ responseStatus response
    else do
    let body = responseBody response
        doc = parseLBS body
        cursor = fromDocument doc
    return $ concatMap (attribute "href") $ cursor $// element "a"

-- | use a predicate filter to list certain files in an http directory
httpGlob :: Manager -> String -> (Text -> Bool) -> IO [Text]
httpGlob mgr url test =
  filter test <$> httpDirectory mgr url

-- | Try to get the filesize (Content-Length) of an http file
httpFileSize :: Manager -> String -> IO (Maybe Int)
httpFileSize mgr url = do
  request <- parseRequest url
  response <- httpNoBody (request {method = "HEAD"}) mgr
  if statusCode (responseStatus response) /= 200
    then error $ show $ responseStatus response
    else do
    let headers = responseHeaders response
    return $ read . B.unpack <$> lookup hContentLength headers

-- | returns the list of http redirects for an url in reverse order (ie last redirect is first)
httpRedirects :: Manager -> String -> IO [B.ByteString]
httpRedirects mgr url = do
  request <- parseRequest url
  respHist <- responseOpenHistory (request {method = "HEAD"}) mgr
  return $ reverse $ mapMaybe (lookup "Location" . responseHeaders . snd) $ hrRedirects respHist

-- | return final redirect for an url
httpRedirect :: Manager -> String -> IO (Maybe B.ByteString)
httpRedirect mgr url =
  listToMaybe <$> httpRedirects mgr url
