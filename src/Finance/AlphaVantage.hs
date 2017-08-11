{-# LANGUAGE OverloadedStrings #-}

module Finance.AlphaVantage (
    daily
  , module Finance.AlphaVantage.Types
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Finance.AlphaVantage.Types
import Network.HTTP.Client

renderFunction :: Function -> ByteString
renderFunction Daily = "TIME_SERIES_DAILY"

renderSymbol :: Symbol -> ByteString
renderSymbol (Symbol sym) = TE.encodeUtf8 sym

renderOutputSize :: OutputSize -> ByteString
renderOutputSize Compact = "compact"
renderOutputSize Full = "full"

renderOutputFormat :: OutputFormat -> ByteString
renderOutputFormat CSV = "csv"
renderOutputFormat JSON = "json"

createRequest :: (MonadIO m, MonadThrow m) => Manager -> Function -> Symbol -> OutputFormat -> APIKey -> Maybe OutputSize -> m (Either String (Vector Bar))
createRequest man fun sym fmt (APIKey key) size = do
  req <- parseRequest "https://www.alphavantage.co/query" >>= return . setQueryString params >>= \ x -> return x{responseTimeout = responseTimeoutNone}
  liftIO $ withResponse req man $ \ response -> do
    case fmt of
      CSV -> do
        body <- brConsume $ responseBody response
        return $ decode HasHeader $ BL.fromStrict $ B.concat body

  where
    params = [("apikey", Just $ TE.encodeUtf8 $ key),("function", Just $ renderFunction fun), ("symbol", Just $ renderSymbol sym),
              ("outputsize",fmap renderOutputSize size), ("datatype", Just $ renderOutputFormat fmt)]

daily :: (MonadIO m, MonadThrow m) => Manager -> Symbol -> Maybe OutputFormat -> APIKey -> Maybe OutputSize -> m (Either String (Vector Bar))
daily man sym fmt key size = createRequest man Daily sym (maybe CSV id fmt) key size
