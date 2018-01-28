{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Finance.AlphaVantage.Types (
    APIKey (..)
  , Bar (..)
  , Function (..)
  , IntradayInterval (..)
  , OutputFormat (..)
  , OutputSize (..)
  , Symbol (..)
  ) where

import Control.Monad
import qualified Data.ByteString as B
import Data.Csv
import Data.Decimal
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format
import Data.Time.LocalTime (LocalTime)

newtype Symbol = Symbol Text deriving (Eq, Ord, Show)

data OutputSize = Compact | Full deriving (Eq, Show)

data OutputFormat = CSV | JSON deriving (Eq, Show)

data Bar = Bar {
  bTimestamp :: LocalTime,
  bOpen :: Decimal,
  bHigh :: Decimal,
  bLow :: Decimal,
  bClose :: Decimal,
  bVolume :: Int
} deriving (Show)

instance FromField Decimal where
  parseField = pure . read . T.unpack . TE.decodeUtf8

instance FromField LocalTime where
  parseField x = case parseTimeM True defaultTimeLocale "%F" $ T.unpack $ TE.decodeUtf8 x of
                   Just y -> return y
                   Nothing -> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ T.unpack $ TE.decodeUtf8 x

instance FromRecord Bar where
  parseRecord v
    | length v == 6 = Bar <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5
    | otherwise = mzero

newtype APIKey = APIKey Text

data Function = Daily | DailyAdjusted | Intraday deriving (Eq)

data IntradayInterval = OneMinute | FiveMinute | FifteenMinute | ThirtyMinute | SixtyMinute deriving (Eq)
