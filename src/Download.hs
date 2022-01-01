{-# LANGUAGE OverloadedStrings #-}

module Download where

import Data.ByteString as BS hiding (head, take, writeFile)
import Data.ByteString.Lazy as BSL hiding (head, take, writeFile)
import qualified Data.ByteString.Lazy as BSL hiding (head, take, writeFile)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding
import Data.Text.Lazy as DL hiding (head, take)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.HTML.DOM
import Text.XML hiding (writeFile)
import Text.XML.Cursor
import Text.Show.Pretty

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest "https://movie.douban.com/top250"
  response <- httpLbs request manager
  let body = responseBody response
      tbody = decodeUtf8 $ BSL.toStrict body
      tDocument = parseLT $ DL.fromStrict tbody
      tCursor = checkNode isMeta $ fromDocument tDocument
  writeFile "result1" (ppShow tCursor)
  -- pPrint tCursor -- checkNode isMeta tCursor

isMeta :: Node -> Bool
isMeta n = case n of
  NodeElement e ->  True -- elementName e == "meta"
  _ -> False