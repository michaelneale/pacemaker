{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module EventGenerator where

import Data.Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Database.Redis as R
import Data.Text.Encoding
import Data.IORef
import qualified Pacemaker as P
import qualified KeyConfig as C
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified HistoricalData as HD
import qualified WebHookNotifier as WH



-- this exists to keep a regular history to report on
-- additionally, if a heartbeat is missing, it will be noted here.

-- super useful for avoiding "value" : null in json output from aeson. 
objectNoNulls :: [AT.Pair] -> Value        
objectNoNulls = object . filter (\(_, v) -> v /= Null)


paceMakeEntity :: PaceConn -> C.APIKeyMap -> T.Text -> IO ()
paceMakeEntity conn apiConf entity = do
  httpBeats <- getHeartbeats conn entity "http"
  heartBeats <- getHeartbeats conn entity "heartbeat"
  currentTime <- P.currentTimeS
  _ <- mapM (expiredCheck (CurrentTime currentTime) entity conn) 
                     (catMaybes (httpBeats ++ heartBeats))    
  _ <- mapM (alertCheck currentTime entity conn apiConf) (catMaybes (httpBeats ++ heartBeats))                  
                     
  return ()                   



-- may want to check if there are any alerts to dispatch
alertCheck :: Integer -> C.Entity -> PaceConn -> C.APIKeyMap -> P.StoredHeartbeat -> IO ()
alertCheck currentTime entity (PaceConn conn) apiConf shb =   
    WH.maybeAlert currentTime entity conn shb $ C.lookupHook entity apiConf
  
  
expiredCheck :: CurrentTime -> C.Entity -> PaceConn -> P.StoredHeartbeat -> IO ()
expiredCheck currentTime entity (PaceConn conn) shb =   
  if hasExpired    
  then 
     do
        putStrLn "Expired check"
        print shb
        HD.storeHistory conn (P.historicalKey entity hb) HD.HistoryMetric {
            HD.current = Nothing,
            HD.predicted = Nothing,
            HD.timestamp = currentTimeSec,
            HD.state = outStatus      
        }
        P.updateCalculatedState entity conn shb outStatus    
        maybeGarbageCollect currentTime entity conn shb        
  else 
    return ()
  where 
      hb = P.heartBeat shb
      (CurrentTime currentTimeSec) = currentTime
      (outStatus, _) = calcState shb currentTime 
      hasExpired = canExpire hb && currentTimeSec >= P.expires shb


maybeGarbageCollect :: CurrentTime -> C.Entity -> R.Connection -> P.StoredHeartbeat -> IO ()
maybeGarbageCollect currentTime entity conn shb =  
  if shouldGC currentTime shb
  then 
    do 
      putStrLn $ "Garbage collecting old check " ++ (show shb)
      P.removeCheck conn entity (P.check hb) (fromMaybe "heartbeat" (P.heartbeat_type hb))
  else
    return ()
  where
      hb = (P.heartBeat shb)
      

-- Should garbage collect after say 2 hours (this is after the check has already expired)
shouldGC :: CurrentTime -> P.StoredHeartbeat -> Bool
shouldGC (CurrentTime now) shb = now > (P.lastSeen shb) + (60*120) 


calcState :: P.StoredHeartbeat -> CurrentTime -> (T.Text, Maybe T.Text)
calcState shb (CurrentTime currentTime) =
  case (reportedState, transitionTo) of 
     ("OK", Nothing) -> 
        if hasExpired
        then ("UNKNOWN", P.summary hb)--Just (T.concat ["Heartbeat expired on: ", lastSeenBeat]))
        else ("OK", P.summary hb)
     (_, Just nextState) -> 
        if hasExpired
        then (nextState, P.summary hb)--Just (T.concat ["Heartbeat expired on: ", lastSeenBeat]))
        else (reportedState, P.summary hb)      
     (_,_) -> (reportedState, P.summary hb)
  where 
    hb = (P.heartBeat shb)
    reportedState = (P.calculatedState shb)
    hasExpired = canExpire hb && currentTime >= P.expires shb
    transitionTo = P.transition_to hb
    --lastSeenBeat = T.pack $ show $ P.expires shb    

canExpire :: P.InboundHeart -> Bool
canExpire hb = maybe True (>= 0) $ P.ttl hb 

main :: P.PaceRedis -> IORef C.APIKeyMap -> IO ()
main (P.PaceRedis paceRedis) api_keys = do
  putStrLn "Starting event generator..."
  rConn <- PaceConn <$> R.connect paceRedis
  _ <- runForever api_keys rConn
  putStrLn "World Ending"

getHeartbeats :: PaceConn -> C.Entity -> T.Text -> IO [Maybe P.StoredHeartbeat]
getHeartbeats (PaceConn conn) entity checkType = do
    res <- R.runRedis conn . R.hgetall . encodeUtf8 $ T.concat [entity, ":", checkType]
    either  (\err -> print err >> return []) 
            (return . map (toStoredHeartbeats . snd)) 
            res
    where
      toStoredHeartbeats = decode . B.fromStrict 


runForever :: IORef C.APIKeyMap -> PaceConn -> IO ThreadId
runForever apikeysRef rConn = 
  forever $ do       
    _ <-  forkIO $ do
        apikeys <- readIORef apikeysRef
        putStrLn "--- starting event generation ---"
        _ <- mapM (paceMakeEntity rConn apikeys) (C.entities apikeys) 
        putStrLn "--- finished event generation ---"
    threadDelay 10000000 -- 10 secs and then kick off a pulse again
        

newtype PaceConn = PaceConn R.Connection
newtype CurrentTime = CurrentTime Integer
