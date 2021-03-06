{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Simple adhoc ixset search indexer and web ui. Document database
contains word chunks of given file, separating the contents with
spaces and couple punctuation characters.  May not so much useful with
documents that not using space as separator.

Since the server is loading the whole set of indice, the first query
after running the server will be slow. From the second query, it will
be better.

When index command has specified with '.html' suffix, the body
contents of html document would be stored in database for searching.

-}
module Main where

import Prelude hiding ((.),id,div,head,span)

import Control.Category
import Control.Exception (evaluate)
import Control.Parallel
import Control.Monad
import Data.Char (toLower)
import Data.List hiding (head,insert,find,span)
import Data.Map (Map)
import Data.Ord
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath
import Text.Printf (printf)

import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Control.Monad.Trans (liftIO)
import Data.Acid
import Data.Iteratee (run, stream2stream)
import Data.Iteratee.IO (enumFile)
import Data.IxSet
import Data.SafeCopy hiding (extension)
import Data.Text (Text)
import Happstack.Server hiding (body, method, port)
import System.Console.CmdArgs hiding (name)
import System.FilePath.Find
import Text.Blaze.Html5 hiding (base,map,summary)
import Text.Blaze.Html5.Attributes hiding
  (dir,id,title,form,style,span,size,start,summary)
import Text.HTML.TagSoup (Tag(..), (~==), innerText, parseTags, sections)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Happstack.Server as H
import qualified Text.Blaze.Html5.Attributes as A

------------------------------------------------------------------------------
-- Database

data Document = Document
  { docPath :: DocPath
  , docWordCount :: Int
  , docWordMap :: Map Text Int
  , docContents :: Contents
  } deriving (Eq,Ord,Data,Typeable)

instance Show Document where
  show (Document (DocPath dp) _ _ _) = "Document " ++ dp

newtype DocPath = DocPath {unDocPath::FilePath}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype Contents = Contents {unContents::Text}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype Word = Word {unWord::Text}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype DocDB = DocDB {docIx :: IxSet Document}
  deriving (Eq,Show,Data,Typeable)

instance Indexable Document where
  empty = ixSet
    [ ixFun (return . docPath)
    , ixFun (map Word . nub . mkChunks . unContents . docContents) ]

mkChunks :: Text -> [Text]
mkChunks = T.split (`elem` " \t\n.,!?&()[]{}<>;/\"'")

deriveSafeCopy 0 'base ''Document
deriveSafeCopy 0 'base ''DocPath
deriveSafeCopy 0 'base ''Contents
deriveSafeCopy 0 'base ''Word
deriveSafeCopy 0 'base ''DocDB

saveDoc :: IxSet Document -> Update DocDB ()
saveDoc ixs = put (DocDB ixs)

loadDoc :: Query DocDB DocDB
loadDoc = ask

makeAcidic ''DocDB ['saveDoc, 'loadDoc]

getDB :: FilePath -> IO DocDB
getDB filepath = do
  acid <- openLocalStateFrom filepath (DocDB empty)
  query acid LoadDoc

------------------------------------------------------------------------------
-- Indexer

index :: FilePath -> Maybe String -> FilePath -> IO ()
index root extn outpath = do
  acid <- openLocalStateFrom outpath (DocDB empty)
  let mkIx = case extn of
        Just e | "html" `isSuffixOf` e -> mkHtmlDocument
               | otherwise             ->
                 mkDocument (fileName ~~? ('*':e)) T.pack
        Nothing -> mkDocument always T.pack
  ixs <- mkIx root
  putStrLn "Creating index .... "
  update acid (SaveDoc ixs)
  closeAcidState acid
  putStrLn "Done."

mkDocument :: FilterPredicate -> (String -> Text)
           -> FilePath -> IO (IxSet Document)
mkDocument cond f root = foldM go empty =<< find always cond root where
  go acc fi = do
    contents <- f `fmap` work fi
    let dp = DocPath ("static" </> drop (length root + 1) fi)
        dc = Contents contents
        ws = mkChunks contents
        dm = foldr (\w m -> M.insertWith (+) w 1 m) M.empty ws
        document = Document dp (length ws) dm dc
    return $! document `par` (acc `pseq` insert document acc)

work :: FilePath -> IO String
work filepath = do
  putStrLn $ "Reading: " ++ filepath
  run =<< enumFile 8192 filepath stream2stream

htmlBody :: String -> Text
htmlBody =
  T.pack . innerText . join . sections (~== TagOpen "body" []) .
  parseTags . map toLower

mkHtmlDocument :: FilePath -> IO (IxSet Document)
mkHtmlDocument = mkDocument (extension ==? ".html") htmlBody

------------------------------------------------------------------------------
-- Server

serve :: Int -> FilePath -> DocDB -> IO ()
serve portnum stt docdb = do
  putStrLn $ "Starting server with port: " ++ show portnum
  simpleHTTP nullConf {H.port=portnum} $ msum
    [ dir "favicon.ico" $
        serveFile (asContentType "http://image/vnd.microsoft.icon")
        "favicon.ico"
    , dir "static" $ serveDirectory EnableBrowsing [] stt
    , searchPage docdb
    , seeOther "" $ toResponse () ]

searchPage :: DocDB -> ServerPartT IO Response
searchPage docdb = do
  qs <- getDataFn $ look "q"
  case qs of
    Left _ ->
      ok $ toResponse $ do
        preEscapedString "<!doctype html>"
        html $ do
          headWithTitle "ixfts - home"
          body $ do
            div ! class_ (toValue "wrapper") $ (about >> inputForm "")
    Right qs' -> do
      (diff,res) <- liftIO $ do
        start <- getPOSIXTime
        ix <- evaluate $
          docIx docdb @* (map Word .  T.words . T.pack $ map toLower qs')
        end <- getPOSIXTime
        return (1000 * realToFrac (end-start) :: Double, ix)
      ok $ toResponse $ do
        preEscapedString "<!doctype html>"
        html $ do
          headWithTitle $ "ixfts - search result for '" ++ qs' ++ "'"
          body $ do
            div ! class_ (toValue "wrapper") $ do
              about
              inputForm qs'
              div ! class_ (toValue "summary") $ toHtml $
                "search result for: \"" ++ qs' ++ "\", " ++
                "hit: " ++ show (size res) ++
                " (" ++ printf "%0.4f ms" diff ++ ")"
              ul $ mapM_ mkLink $ sortBy (comparing $ score qs') $ toList res

headWithTitle :: String -> Html
headWithTitle str = head $ title (toHtml str) >> favicon >> css

score :: String -> Document -> Double
score ws document = foldr f 0 (T.words $ T.pack ws) where
  f w acc = acc + (total / fromIntegral (M.findWithDefault 0 w dmap))
  dmap = docWordMap document
  total = fromIntegral $ docWordCount document

mkLink :: Document -> Html
mkLink document =
  li $ do
    a ! href (toValue filepath) $ do
      toHtml $ dropWhile (/= '/') filepath
  where
    filepath = unDocPath $ docPath document

inputForm :: String -> Html
inputForm val =
  div ! class_ (toValue "input_form") $ do
    form !
      enctype (toValue "multipart/form-data") !
      method (toValue "GET") !
      action (toValue "/") $ do
        input !
          type_ (toValue "text") !
          name (toValue "q") !
          A.size (toValue "40") !
          value (toValue val)
        input !
          type_ (toValue "submit") !
          value (toValue "search")
        a !
          class_ (toValue "src") !
          href (toValue "https://github.com/8c6794b6/ixfts") $ toHtml "src"

css :: Html
css = style ! type_ (toValue "text/css") $ toHtml
 "body { font-size: 15px; } \
\input { margin: 5px; border: 1px solid #868686; }\
\div.wrapper { padding: 20px; } \
\div.wrapper div.about {padding-left: 8px}\
\div.wrapper div.about span.desc {font-size: 75%}\
\div.wrapper ul { list-style: none; padding-left: 10px; } \
\div.wrapper ul li { margin: 5px 0 } \
\div.wrapper ul li a { text-decoration: none; } \
\div.wrapper a.src {font-size: 75%}\
\div.summary { font-size: 75%; padding-left: 20px; }"

favicon :: Html
favicon =
  link !
    rel (toValue "shortcut icon") !
    type_ (toValue "image/vnd.microsoft.icon") !
    href (toValue "/favicon.ico")

about :: Html
about = div ! class_ (toValue "about") $ do
  toHtml "ixfts "
  span ! class_ (toValue "desc") $ toHtml "- minimalistic html search"


------------------------------------------------------------------------------
-- CLI

data IxFts
  = Index { doc :: FilePath
          , ext :: Maybe String
          , out :: String }
  | Serve { port :: Int
          , db :: String
          , static :: FilePath }
    deriving (Eq,Show,Data,Typeable)

commands :: IxFts
commands = modes
  [ Index
      { doc = def &= typDir &=
            help "Path to directory containing target documents"
      , ext = def &= typ "EXTENSION" &=
              help "File extension to read"
      , out = "state" &= typDir &=
              help "Path to output index directory (default:state)"
      } &= help "Index documents under given path"
  , Serve
      { port = 8000 &= typ "PORT" &=
           help "Port number to serve (default:8000)"
      , db = "state" &= typDir &=
           help "Path to index database (default:state)"
      , static = "static" &= typDir &=
                 help "Path to static contents directory (default:static)"
      } &= help "Start HTTP server"
  ] &= summary "ixfts: simple text search indexer and server"

main :: IO ()
main = do
  arguments <- cmdArgs commands
  case arguments of
    Index inpath e o -> index inpath e o
    Serve pn d s -> getDB d >>= \acid -> acid `seq` serve pn s $! acid
