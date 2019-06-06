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
         httpRawDirectory,
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
import Data.List (nub)
import Data.Maybe
import Data.Text (Text, isPrefixOf, pack)
import Data.Time.Clock (UTCTime)

import Network.HTTP.Client (hrRedirects, httpLbs, httpNoBody, Manager, method,
                            newManager, parseRequest,
                            Request, Response, responseBody, responseHeaders,
                            responseOpenHistory, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Date (httpDateToUTC, parseHTTPDate)
import Network.HTTP.Types (hContentLength, hLocation, methodHead, statusCode)

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
--
-- (Filters "non-files/subdirs" @since 0.1.4 (before that was just httpRawDirectory)
httpDirectory :: Manager -> String -> IO [Text]
httpDirectory mgr url = do
  hrefs <- httpRawDirectory mgr url
  return $ nub $ filter (not . or . flist [isHttp, ("/" `isPrefixOf`), (pack "../" ==)]) hrefs
  where
    isHttp loc = "http:" `isPrefixOf` loc || "https:" `isPrefixOf` loc

-- picked from swish
flist :: [a->b] -> a -> [b]
flist fs a = map ($ a) fs

-- | List all the hrefs in an http directory html file.
--
-- Raises an error if the http request fails.
--
-- Note if the directory (webpage) url is redirected to a different path
-- you may need to use 'httpRedirect' to determine
-- the actual final url prefix for relative links
-- (files).
--
-- @since 0.1.4
httpRawDirectory :: Manager -> String -> IO [Text]
httpRawDirectory mgr url = do
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
  response <- httpHead mgr url
  return $ statusCode (responseStatus response) == 200

-- | Try to get the filesize (Content-Length field) of an http file
--
-- Raises an error if the http request fails.
httpFileSize :: Manager -> String -> IO (Maybe Integer)
httpFileSize mgr url = do
  response <- httpHead mgr url
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
  response <- httpHead mgr url
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
  request <- parseRequestHead url
  respHist <- responseOpenHistory request mgr
  return $ reverse $ mapMaybe (lookup hLocation . responseHeaders . snd) $ hrRedirects respHist

-- | Return final redirect for an url
httpRedirect :: Manager -> String -> IO (Maybe B.ByteString)
httpRedirect mgr url =
  listToMaybe <$> httpRedirects mgr url

-- parseRequest with HEAD
--
-- @since 0.1.3
parseRequestHead :: String -> IO Request
parseRequestHead url = do
  request <- parseRequest url
  return $ request {method = methodHead}

-- @since 0.1.3
httpHead :: Manager -> String -> IO (Response ())
httpHead mgr url = do
  request <- parseRequestHead url
  httpNoBody request mgr

