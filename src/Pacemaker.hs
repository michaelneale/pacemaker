{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Pacemaker where

import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import qualified Database.Redis as R
import Data.Text.Encoding
import Data.Time.Clock.POSIX

import qualified Data.ByteString as BS
import qualified Metrics as M
import qualified HistoricalData as HD

{-- 
Collect incoming heartbeats. 
Some may have metrics, some just OK/CRITICAL states. 
If they go missing - they become UNKNOWN state (ish). 

An example inbound heartbeat:
{
"ttl" : 400,
"state" : "OK",
"elapsed" : 110,
"tags" : ["blah"],
"summary" : "OK",
"api_key" : "1234",
"check" : "https://www.google.com.au"
}
--}

data InboundHeart = 
    InboundHeart {
      api_key :: Text, 
      check :: Text,
      state :: Maybe Text,        
      ttl :: Maybe Integer,
      elapsed :: Maybe Integer,       
      tags :: Maybe [Text],
      summary :: Maybe Text,
      heartbeat_type :: Maybe Text,
      transition_to :: Maybe Text,
      metric :: Maybe Double
} deriving (Show, Generic)

data StoredHeartbeat = 
  StoredHeartbeat {
    heartBeat :: InboundHeart,
    expires :: Integer,
    lastSeen :: Integer,
    calculatedState :: Text
  } deriving (Show, Generic)

instance ToJSON InboundHeart
instance FromJSON InboundHeart
instance ToJSON StoredHeartbeat
instance FromJSON StoredHeartbeat


data PaceResponse = PaceResponse { status :: Text }
  deriving (Show, Generic) 
instance ToJSON PaceResponse


pingbotBeat :: Text -> R.Connection -> InboundHeart -> IO PaceResponse
pingbotBeat entity conn ib = do
  (storedHB, lr) <- makeStored conn keys ib  
  res <- R.runRedis conn $ R.hset (_storageKey keys) (_hashKey keys) (B.toStrict (encode storedHB))    
  HD.storeHistory conn (historicalKey entity ib) (toHistorical storedHB lr)
  case res of 
    Left err -> do 
          print err
          return $ PaceResponse "Unable to store. Try again shortly."
    Right _ -> return $ PaceResponse $ calculatedState storedHB
  where 
    keys = toStorageKey ib entity 
    

updateCalculatedState :: Text -> R.Connection -> StoredHeartbeat -> Text -> IO () 
updateCalculatedState entity conn shb newState = do 
  res <- R.runRedis conn $ R.hset (_storageKey keys) (_hashKey keys) (B.toStrict (encode newRec))    
  case res of 
    Left err -> do 
          putStrLn "Unable to update calculated state"
          print err
          return ()
    Right _ -> return ()  
  where  
    newRec = shb { calculatedState = newState }
    keys = toStorageKey (heartBeat shb) entity


historicalKey :: Text -> InboundHeart -> BS.ByteString
historicalKey entity ib = encodeUtf8 $ Data.Text.concat [entity, ":", (check ib)]

toStorageKey :: InboundHeart -> Text -> StorageKeys
toStorageKey ib entity = 
    StorageKeys { _storageKey = encodeUtf8 $ makeKey ib entity,
                  _hashKey = encodeUtf8 $ check ib }
                                

toHistorical :: StoredHeartbeat -> Maybe M.LearnResponse -> HD.HistoryMetric
toHistorical shb lr = 
  HD.HistoryMetric {
      HD.current = (metric (heartBeat shb)),
      HD.predicted = maybe Nothing M.predicted lr, 
      HD.timestamp = (lastSeen shb),
      HD.state = (calculatedState shb)      
  }
  


removeCheck :: R.Connection -> Text -> Text -> Text -> IO ()
removeCheck conn entity _check heartbeatType = do
  res <- R.runRedis conn $ R.hdel (_storageKey keys) [(_hashKey keys)]
  _ <- R.runRedis conn $ R.del [encodeUtf8 $ Data.Text.concat [entity, ":", _check]]
  case res of 
    Left err -> do 
          print err
    Right x -> putStrLn $ "Removed " ++ (show x) 
  where
    keys = StorageKeys { _storageKey = encodeUtf8 $ Data.Text.concat [entity, ":", heartbeatType],
                         _hashKey = encodeUtf8 _check}
    


makeStored :: R.Connection -> StorageKeys -> InboundHeart -> IO (StoredHeartbeat, Maybe M.LearnResponse)
makeStored conn keys ib = do 
    current <- currentTimeS 
    nextExpiry <- calcNextExpiry current conn keys ib
    (calcSt, learned) <- calcState ib keys
    return $ ((StoredHeartbeat ib nextExpiry current calcSt), learned)

calcState :: InboundHeart -> StorageKeys -> IO (Text, Maybe M.LearnResponse)
calcState ib keys = 
  case ((state ib), (metric ib)) of
    (Just st, _) -> return (st, Nothing)
    (Nothing, Just mt) -> learnFromMetric mt keys
    _ -> return ("UNKNOWN", Nothing)
    
    
learnFromMetric :: M.MetricValue -> StorageKeys -> IO (Text, Maybe M.LearnResponse)
learnFromMetric metricVal keys = do
      learnSvc <- M.getLearnService
      current <- M.currentTimeS
      lr <- M.doLearn learnSvc (M.toLearnReq current metricVal (makeCheckId keys)) 
      return $ (M.calculatedState lr, lr)

makeCheckId :: StorageKeys -> Text
makeCheckId keys = Data.Text.concat [key1, key2]            
  where 
      key1 = decodeUtf8 (_storageKey keys)
      key2 = decodeUtf8 (_hashKey keys)  

calcNextExpiry :: Integer -> R.Connection -> StorageKeys -> InboundHeart -> IO Integer
calcNextExpiry current conn keys ib = do 
    case ttl ib of 
      Just _ -> return $ calcExpiry ib current
      _ -> do
            res <- R.runRedis conn $ R.hget (_storageKey keys) (_hashKey keys)
            case res of  
              Right (Just j) -> return $ loadSHB current ib j
              _ -> do 
                    putStrLn "(applying default initial TTL)"
                    return $ calcExpiry ib current


loadSHB :: Integer -> InboundHeart -> BS.ByteString -> Integer
loadSHB current ib j = case (decode (B.fromStrict j)) of 
    Just shb -> adaptiveExpected (lastSeen shb) (expires shb) current
    _ -> calcExpiry ib current

  
calcExpiry :: InboundHeart -> Integer -> Integer  
calcExpiry ib current = 
  current + maybe (400) (\a -> a) (ttl ib)

makeKey :: InboundHeart -> Text -> Text 
makeKey ib entity = 
  Data.Text.concat [entity, ":", maybe ("heartbeat") (\ht -> ht) (heartbeat_type ib)] 

checkState :: InboundHeart -> Bool
checkState ib = 
  case ((state ib), (metric ib)) of
    (Just "OK", _) -> True
    (Just "CRITICAL", _) -> True    
    (Nothing, Nothing) -> False    
    (Nothing, _) -> True
    _ -> False


-- all in epoch time - returns expected epoch time
adaptiveExpected :: Integer -> Integer -> Integer -> Integer
adaptiveExpected lastSeenTime expectedTime currentTime = 
      if currentTime > expectedTime 
      then 
        currentTime + cap ((currentTime - lastSeenTime) `by` 1.2)
      else 
        if (expectedTime - currentTime) > tenPercent
        then
          currentTime + (effectiveTTL `by` 0.9)
        else
          currentTime + effectiveTTL
    where 
      effectiveTTL = expectedTime - lastSeenTime
      tenPercent = effectiveTTL `by` 0.1


by :: Integer -> Double -> Integer
by a m = round $ fromIntegral a * m
        
cap :: Integer -> Integer
cap toAdd = min toAdd 400

currentTimeS :: IO Integer
currentTimeS = round `fmap` getPOSIXTime 


newtype PaceRedis = PaceRedis R.ConnectInfo
data StorageKeys = StorageKeys { _storageKey :: BS.ByteString, _hashKey :: BS.ByteString }
      
