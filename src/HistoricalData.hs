{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module HistoricalData where

import Data.Aeson
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics
import qualified Database.Redis as R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified KeyConfig as C

data HistoryMetric = 
  HistoryMetric {
    current :: Maybe Double, 
    predicted :: Maybe Double,
    timestamp :: Integer,
    state :: T.Text
  } deriving (Show, Generic)
  
instance ToJSON HistoryMetric
instance FromJSON HistoryMetric

storeHistory :: R.Connection -> BS.ByteString -> HistoryMetric -> IO ()
storeHistory conn key metric = do 
  res <- R.runRedis conn $ R.lpush key [B.toStrict (encode metric)]
  res' <- R.runRedis conn $ R.ltrim key 0 500
  case (res, res') of 
    (Right _, Right _) -> return ()
    (str, trimming) -> do
        print str
        print trimming
        putStrLn "Problems storing metric"

listHistory :: R.Connection -> C.Entity -> T.Text -> IO [Maybe HistoryMetric]
listHistory conn entity check = do
    res <- R.runRedis conn $ R.lrange histKey 0 (-1)
    either  (\err -> print err >> return []) 
            (return . map toHist) 
            res
    where
      toHist = decode . B.fromStrict 
      histKey = encodeUtf8 $ T.concat [entity, ":", check]
