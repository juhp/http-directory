{-# LANGUAGE CPP #-}

module Network.HTTP.Directory
       ( httpDirectory,
         httpRedirect,
         httpRedirects,
--         httpGlob,
--         httpFileSize
       ) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Text (Text)

import Network.HTTP.Client (hrRedirects, httpLbs, Manager, method, parseRequest,
                            responseBody, responseHeaders, responseOpenHistory,
                            responseStatus)
import Network.HTTP.Types (statusCode)

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

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

-- | returns the list of http redirects for an url
httpRedirects :: Manager -> String -> IO [B.ByteString]
httpRedirects mgr url = do
  request <- parseRequest url
  respHist <- responseOpenHistory (request {method = "HEAD"}) mgr
  return $ reverse $ mapMaybe (lookup "Location" . responseHeaders . snd) $ hrRedirects respHist

-- | return final redirect for an url
httpRedirect :: Manager -> String -> IO (Maybe B.ByteString)
httpRedirect mgr url =
  listToMaybe <$> httpRedirects mgr url
