{-# LANGUAGE OverloadedStrings #-}

module Download where

import           Control.Monad
import           Data.ByteString               as BS
                                         hiding ( concatMap
                                                , head
                                                , take
                                                , writeFile
                                                )
import           Data.ByteString.Lazy          as BSL
                                         hiding ( concatMap
                                                , head
                                                , take
                                                , writeFile
                                                )
import qualified Data.ByteString.Lazy          as BSL
                                         hiding ( concatMap
                                                , head
                                                , take
                                                , writeFile
                                                )
import           Data.Foldable
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.Encoding
import qualified Data.Text.IO                  as TIO
import           Data.Text.Lazy                as DL
                                         hiding ( concatMap
                                                , head
                                                , take
                                                )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Text.HTML.DOM
import           Text.Show.Pretty
import           Text.XML                hiding ( writeFile )
import           Text.XML.Cursor

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  TIO.writeFile "result1" ""
  for_ [0, 25 .. 250-25] $ \i -> do
    request  <- parseRequest $ "https://movie.douban.com/top250?start=" ++ show i ++  "&filter=" 
    response <- httpLbs request manager
    let body      = responseBody response
        tbody     = decodeUtf8 $ BSL.toStrict body
        tDocument = parseLT $ DL.fromStrict tbody
        tCursor   = front1 $ fromDocument tDocument
    TIO.appendFile "result" (T.concat $ fmap unDF tCursor)
  -- TIO.writeFile "result1" (T.concat $ (fmap (T.pack . ppShow)) tCursor)

data DF a
  = F1 {unDF :: a }
  | F2 {unDF :: a }
  | F3 {unDF :: a }
  | F4 {unDF :: a }
  | F5 {unDF :: a }
  deriving Show

-- unDF :: DF a -> a
-- unDF 

front1 =
  descendant
    >=> element "body"
    >=> dea "div" "id" "wrapper"
    >=> dea "div" "id" "content"
    >=> de "div"
    >=> hasAttribute "class"
    >=> dea "div" "class" "article"
    >=> dea "ol"  "class" "grid_view"
    >=> de "li"
    >=> dea "div" "class" "item"
    >>: [ dea "div" "class" "pic"
        >=> de "a"
        >=> de "img"
        >=> attribute "src"
        |:  (F1 . (<> "\n"))
        , dea "div" "class" "info"
          >>: [ dea "div" "class" "hd"
              >=> de "a"
              >>: [ dea "span" "class" "title"
                  >=> descendant
                  >=> content
                  |:  (F2 . handleText)
                  , dea "span" "class" "other"
                  >=> descendant
                  >=> content
                  |:  (F3 . handleText)
                  ]
              , dea "div" "class" "bd"
                >>: [ de "p" >=> descendant >=> content |: (F4 . handleText)
                    , de "div"
                    >=> de "span"
                    >=> descendant
                    >=> content
                    |:  (F5 . handleText)
                    ]
              ]
        ]


-- >>> main

dea a b c = descendant >=> element a >=> attributeIs b c

de a = descendant >=> element a


infixr 7 |:

(|:) :: Functor m => (b -> m c) -> (c -> d) -> (b -> m d)
(|:) a f va = f <$> a va

infixr 2 >>:

(>>:) :: Monad m => (a -> m b) -> m (b -> m c) -> a -> m c
(va >>: fs) a = do
  f <- fs
  (va >=> f) a

handleText :: T.Text -> T.Text
handleText = (<> "\n") . T.stripStart . T.stripEnd . T.filter sc

sc :: Char -> Bool
sc ' '  = False
sc '\n' = False
sc ' '  = False
-- sc '/'  = False
sc _    = True
