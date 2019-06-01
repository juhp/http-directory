{-# LANGUAGE CPP #-}

{-|
A library for listing "files" in an http "directory".

@
import Network.HTTP.Directory
import qualified Data.Text as T

main = do
  mgr <- httpManager
  files <- httpDirectory mgr "https://example.com/some/dir/"
  mapM_ T.putStrLn files
  httpFileSize mgr (head files) >>= print
  httpLastModified mgr (head files) >>= print
@
-}

module Network.HTTP.Directory
       ( httpDirectory,
         httpExists,
         httpFileSize,
         httpLastModified,
         httpManager,
         httpRedirect,
         httpRedirects
       ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Network.HTTP.Client (hrRedirects, httpLbs, httpNoBody, Manager, method,
                            newManager, parseRequest, responseBody,
                            responseHeaders, responseOpenHistory, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Date (httpDateToUTC, parseHTTPDate)
import Network.HTTP.Types (hContentLength, hLocation, statusCode)

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

-- | List the file links (hrefs) in an http directory
--
-- Raises an error if the http request fails.
--
-- Note if the directory (webpage) url is redirected to a different path
-- you may need to use 'httpRedirect' to determine
-- the actual final url prefix for relative links
-- (files).
httpDirectory :: Manager -> String -> IO [Text]
httpDirectory mgr url = do
  request <- parseRequest url
  response <- httpLbs request mgr
  if statusCode (responseStatus response) /= 200
    then do
    putStrLn url
    error $ show $ responseStatus response
    else do
    let body = responseBody response
        doc = parseLBS body
        cursor = fromDocument doc
    return $ concatMap (attribute "href") $ cursor $// element "a"

-- | Test if an file (url) exists
--
-- @since 0.1.3
httpExists :: Manager -> String -> IO Bool
httpExists mgr url = do
  request <- parseRequest url
  response <- httpNoBody (request {method = "HEAD"}) mgr
  return $ statusCode (responseStatus response) == 200

-- | Try to get the filesize (Content-Length field) of an http file
--
-- Raises an error if the http request fails.
httpFileSize :: Manager -> String -> IO (Maybe Integer)
httpFileSize mgr url = do
  request <- parseRequest url
  response <- httpNoBody (request {method = "HEAD"}) mgr
  if statusCode (responseStatus response) /= 200
    then do
    putStrLn url
    error $ show $ responseStatus response
    else do
    let headers = responseHeaders response
    return $ read . B.unpack <$> lookup hContentLength headers

-- | Try to get the modification time (Last-Modified field) of an http file
--
-- Raises an error if the http request fails.
--
-- @since 0.1.1
httpLastModified :: Manager -> String -> IO (Maybe UTCTime)
httpLastModified mgr url = do
  request <- parseRequest url
  response <- httpNoBody (request {method = "HEAD"}) mgr
  if statusCode (responseStatus response) /= 200
    then do
    putStrLn url
    error $ show $ responseStatus response
    else do
    let headers = responseHeaders response
        mdate = lookup "Last-Modified" headers
    return $ httpDateToUTC <$> maybe Nothing parseHTTPDate mdate

-- | alias for 'newManager tlsManagerSettings'
-- so one does not need to import http-client etc
--
-- @since 0.1.2
httpManager :: IO Manager
httpManager =
  newManager tlsManagerSettings

-- | Returns the list of http redirects for an url in reverse order
-- (ie last redirect is listed first)
httpRedirects :: Manager -> String -> IO [B.ByteString]
httpRedirects mgr url = do
  request <- parseRequest url
  respHist <- responseOpenHistory (request {method = "HEAD"}) mgr
  return $ reverse $ mapMaybe (lookup hLocation . responseHeaders . snd) $ hrRedirects respHist

-- | Return final redirect for an url
httpRedirect :: Manager -> String -> IO (Maybe B.ByteString)
httpRedirect mgr url =
  listToMaybe <$> httpRedirects mgr url
