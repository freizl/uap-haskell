{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (group, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Directory (getHomeDirectory)
import Text.Printf (printf)
import Web.UAParser (UAResult (..), parseUA)

type Browser = Text

type Widget = ByteString

type Domain = ByteString

type Cell = ByteString

type UAString = ByteString

type UnknownBrowser = (Cell, Domain, Widget, UAString)

type NoneIE11 = (Cell, Domain, Widget, Browser, UAString)

type UACache = Map.Map ByteString (Maybe UAResult)

data SearchResult = SearchResult
  { domain :: Domain,
    siw :: Widget,
    ua :: UAString
  }
  deriving (Generic, Show)

instance FromNamedRecord SearchResult

instance DefaultOrdered SearchResult

data Output = Output
  { browser :: Browser,
    visits :: Int,
    percentage :: String
  }
  deriving (Generic, Show)

instance ToNamedRecord Output

instance DefaultOrdered Output

csvFiles :: [String]
csvFiles =
  [ "ok8-widget-domain-ua.csv",
    "ok1-widget-domain-ua.csv",
    "ok2-widget-domain-ua.csv",
    "ok3-widget-domain-ua.csv",
    "ok4-widget-domain-ua.csv",
    "ok6-widget-domain-ua.csv",
    "ok7-widget-domain-ua.csv"
  ]


resultFile :: String
resultFile = "/tmp/widget-uas.csv"

main :: IO ()
main = do
  let uaParseCache = Map.empty :: UACache
  cacheRef <- newIORef uaParseCache
  result <- mapM (mainFile cacheRef) csvFiles
  let (browserResult, unknownBrowserResult, noneIE11Result) = foldl mergeResult (V.empty, V.empty, V.empty) result
  printIf "unknown browsers" (nub $ sort $ V.toList unknownBrowserResult)
  printIf "not ie 11" (nub $ sort $ V.toList noneIE11Result)
  let total = V.length browserResult
  let finalResult = map (\xs -> calculateBrowserUsage total (head xs, length xs)) $ group $ sort $ V.toList browserResult
  BSL.writeFile resultFile (Data.Csv.encodeDefaultOrderedByName finalResult)
  return ()

mergeResult :: (Vector a1, Vector a2, Vector a3) -> (Vector a1, Vector a2, Vector a3) -> (Vector a1, Vector a2, Vector a3)
mergeResult (a, b, c) (a1, b1, c1) = (a V.++ a1, b V.++ b1, c V.++ c1)

calculateBrowserUsage :: Int -> (Browser, Int) -> Output
calculateBrowserUsage totalCount (browserName, count) =
  Output
    { browser = browserName,
      visits = count,
      percentage = printf "%.5f" (((fromIntegral count / fromIntegral totalCount) * 100) :: Float) ++ "%"
    }

mainFile :: IORef UACache -> String -> IO (V.Vector Browser, V.Vector UnknownBrowser, V.Vector NoneIE11)
mainFile uaCache fileName = do
  homeDir <- getHomeDirectory
  print fileName
  let cell = C.pack $ takeWhile (/= '-') fileName
  file <- BSL.readFile (homeDir <> "/Downloads" <> "/" <> fileName)
  case (Data.Csv.decodeByName file :: Either String (Header, V.Vector SearchResult)) of
    Left e -> print e >> return (V.empty, V.empty, V.empty)
    Right (_, xs) -> do
      ys <- V.mapM (parseUAToBrowserName uaCache) xs
      let unknownBrowserResult = V.map (\(domain, siw, _, raw) -> (cell, domain, siw, raw)) $ V.filter (T.null . thd3) ys
      let notIE11Browsers = V.map (\(domain, siw, browser, raw) -> (cell, domain, siw, browser, raw)) $ V.filter (notIE11 . thd3) ys
      return (V.filter (\x -> notBot x && not (T.null x)) $ V.map thd3 ys, unknownBrowserResult, notIE11Browsers)

parseUAToBrowserName :: IORef UACache -> SearchResult -> IO (Domain, Widget, Browser, UAString)
parseUAToBrowserName uaCache SearchResult {..} = do
  maybeUA <- parseUAOrReadCache uaCache ua
  return (domain, siw, createBrowserName maybeUA, ua)

parseUAOrReadCache :: IORef UACache -> ByteString -> IO (Maybe UAResult)
parseUAOrReadCache uaCache str = do
  cache <- readIORef uaCache
  case Map.lookup str cache of
    Nothing -> do
      let r = parseUA str
      writeIORef uaCache (Map.insert str r cache)
      return r
    Just r -> return r

thd3 :: (a, b, c, d) -> c
thd3 (_, _, c, _) = c

third3 :: (c -> c') -> (a, b, c, d) -> (a, b, c', d)
third3 f (a, b, c, d) = (a, b, f c, d)

createBrowserName :: Maybe UAResult -> Browser
createBrowserName Nothing = ""
createBrowserName (Just UAResult {..}) = maybe uarFamily (\v1 -> if uarFamily == "IE" then uarFamily <> v1 else uarFamily) uarV1

notIE11 :: Text -> Bool
notIE11 browserName = T.isPrefixOf "IE" browserName && browserName /= "IE11"

printIf :: (Foldable t, Show a) => String -> t a -> IO ()
printIf headerStr xs = unless (null xs) (putStrLn headerStr >> mapM_ print xs)

sortByCount :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
sortByCount x y = snd x `compare` snd y

notBot :: Text -> Bool
notBot k = not $ T.isInfixOf "bot" (T.toLower k)
