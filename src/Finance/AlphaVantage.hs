{-# LANGUAGE OverloadedStrings #-}

module Finance.AlphaVantage (
    daily
  , intraday
  , module Finance.AlphaVantage.Types
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Finance.AlphaVantage.Types
import Network.HTTP.Client

renderFunction :: Function -> ByteString
renderFunction Daily = "TIME_SERIES_DAILY"
renderFunction Intraday = "TIME_SERIES_INTRADAY"
renderFunction DailyAdjusted = "TIME_SERIES_DAILY_ADJUSTED"

renderSymbol :: Symbol -> ByteString
renderSymbol (Symbol sym) = TE.encodeUtf8 sym

renderOutputSize :: OutputSize -> ByteString
renderOutputSize Compact = "compact"
renderOutputSize Full = "full"

renderOutputFormat :: OutputFormat -> ByteString
renderOutputFormat CSV = "csv"
renderOutputFormat JSON = "json"

renderIntradayInterval :: IntradayInterval -> ByteString
renderIntradayInterval OneMinute = "1min"
renderIntradayInterval FiveMinute = "5min"
renderIntradayInterval FifteenMinute = "15min"
renderIntradayInterval ThirtyMinute = "30min"
renderIntradayInterval SixtyMinute = "60min"

createRequest :: (MonadIO m, MonadThrow m) => Manager -> Function -> Symbol -> OutputFormat -> APIKey -> Maybe OutputSize -> Maybe IntradayInterval -> m (Either String (Vector Bar))
createRequest man fun sym fmt (APIKey key) size interval = do
  req <- parseRequest "https://www.alphavantage.co/query" >>= return . setQueryString (params ++ foo) >>= \ x -> return x{responseTimeout = responseTimeoutNone}
  liftIO $ withResponse req man $ \ response -> do
    case fmt of
      CSV -> do
        body <- brConsume $ responseBody response
        return $ decode HasHeader $ BL.fromStrict $ B.concat body

  where
    params = [("apikey", Just $ TE.encodeUtf8 $ key),("function", Just $ renderFunction fun), ("symbol", Just $ renderSymbol sym),
              ("outputsize",fmap renderOutputSize size), ("datatype", Just $ renderOutputFormat fmt)]

    foo = catMaybes [fmap(\ x -> ("interval", Just $ renderIntradayInterval x)) interval]

daily :: (MonadIO m, MonadThrow m) => Manager -> Symbol -> Maybe OutputFormat -> APIKey -> Maybe OutputSize -> m (Either String (Vector Bar))
daily man sym fmt key size = createRequest man Daily sym (maybe CSV id fmt) key size Nothing

dailyAdjusted :: (MonadIO m, MonadThrow m) => Manager -> Symbol -> Maybe OutputFormat -> APIKey -> Maybe OutputSize -> m (Either String (Vector Bar))
dailyAdjusted man sym fmt key size = createRequest man DailyAdjusted sym (maybe CSV id fmt) key size Nothing

intraday :: (MonadIO m, MonadThrow m) => Manager -> Symbol -> IntradayInterval -> Maybe OutputFormat -> APIKey -> Maybe OutputSize -> m (Either String (Vector Bar))
intraday man sym interval fmt key size = createRequest man Intraday sym (maybe CSV id fmt) key size (Just interval)
