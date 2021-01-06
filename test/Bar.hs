{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import GHC.Generics ( Generic )
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Web.UAParser ( UAResult(..), parseUA, parseOS, parseDev )
import Data.Maybe ( fromJust, fromMaybe, isJust, isNothing )
import Data.Text (Text)
import qualified Data.Text as T
import Data.List ( (++), map, sort )
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Fold
import Data.Csv
    ( (.:),
      header,
      decodeByName,
      encode,
      DefaultOrdered(..),
      FromNamedRecord(..),
      Header )
import qualified Data.Vector as V
import Control.Monad ( unless )
import Text.Printf ( printf )

type UAName = Text

data SearchResult = SearchResult
  { domain :: Text
  , ua :: Text
  } deriving (Generic, Show)
instance FromNamedRecord SearchResult where
    parseNamedRecord m = SearchResult <$> m .: "DOMAIN" <*> m .: "UA"

instance DefaultOrdered SearchResult where
    headerOrder _ = header ["DOMAIN", "UA"]


csvFiles :: [String]
csvFiles =
  [ "ok1-domain-ua-raw.csv"
  , "ok2-domain-ua-raw.csv"
  , "ok3-domain-ua-raw.csv"
  , "ok4-domain-ua-raw.csv"
  , "ok5-domain-ua-raw.csv"
  , "ok6-domain-ua-raw.csv"
  , "ok7-domain-ua-raw.csv"
  , "ok8-domain-ua-raw.csv"
  , "ok9-domain-ua-raw.csv"
  -- , "ok10-domain-ua-raw.csv"
  , "ok11-domain-ua-raw.csv"
  , "ok12-domain-ua-raw.csv"
  ]

sortByCount :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
sortByCount x y = (snd x) `compare` (snd y)

notBot :: Text -> p -> Bool
notBot k _ = not $ T.isInfixOf "bot" (T.toLower k)

main :: IO ()
main = do
  let initMap = Map.empty :: Map.Map Text [Text]
  finalResult <- Fold.foldrM mainFile initMap csvFiles
  let totalCount = length . concat . Map.elems $ finalResult
  let results = (sort . map (\(a, b) -> (a, b, printf "%.4f" ((fromIntegral b / fromIntegral totalCount * 100) :: Float) ++ "%")) . Map.toList . Map.map length . Map.filterWithKey notBot $ finalResult)
  BSL.writeFile "/tmp/widget-uas.csv" (Data.Csv.encode (results :: [(Text, Int, String)]))
  return ()

mainFile :: String -> Map.Map Text [Text] -> IO (Map.Map Text [Text])
mainFile fileName resultMap = do
  print fileName
  let cell = C.pack $ takeWhile (/= '-') fileName
  file <- BSL.readFile ( "/Users/haishengwu/Downloads/" <> fileName )
  case (Data.Csv.decodeByName file :: Either String (Header, V.Vector SearchResult)) of
    Left e -> print e >> return Map.empty
    Right (_, xs) -> do
      let uas = V.toList $ V.map (\(SearchResult{..}) -> (encodeUtf8 domain, encodeUtf8 ua)) xs
      let parsed = Data.List.map (\(domain, str ) -> (parseUA str, domain, str)) uas
      let addToMap ua' re' = let uar' = uarFamily ua'
                                 v = fromMaybe "" (uarV1 ua')
                                 k = if uar' == "IE" then (uar' <> v) else uar'
                            in if Map.notMember k re' then Map.insert k [v] re' else Map.adjust (Data.List.++ [v]) k re'
      let finalResult = Fold.foldr addToMap resultMap ([fromJust ma | (ma, _, _) <- parsed, isJust ma] :: [UAResult])
      let unknownBrowserResult = [ (cell, domain, raw) | (ma, domain, raw) <- parsed, isNothing ma ]
      let notIE11Browsers = [ (cell, domain, raw) | (ma,domain,raw) <- parsed, notIE11 ma ]
      return finalResult

notIE11 :: Maybe UAResult -> Bool
notIE11 uaResult = (maybe False ((== "IE") . uarFamily) uaResult)
  && (maybe False (/= "11") (uaResult >>= uarV1))

printNonEmptyArray :: Foldable t => String -> t ByteString -> IO ()
printNonEmptyArray headerStr xs = unless (null xs) (putStrLn headerStr >> Fold.mapM_ (\x -> (putStr "\t" >> C.putStrLn x)) xs)

mainTest :: ByteString -> IO ()
mainTest str = do
  print (parseUA str)
  print (parseOS str)
  print (parseDev str)


{-
host=ok5-*crtr* "GET /assets/js/sdk/okta-signin-widget/5.1.2/js/okta-sign-in.min.js" "Mozilla/"
| rex "(?<UA>Mozilla/[^\"]+)[\" -]+(?<DOMAIN>[^ .\"]+\.[^ .\"]+\.[^ .\"]+)"
| table DOMAIN, UA
| sort DOMAIN

Time Range: 12/21/2020 17:00:00 ~ 12/21/2020 21:00:00

-}
