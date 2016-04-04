{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module WebHookNotifier where

import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Maybe
import Control.Concurrent

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified HistoricalData as HS
import qualified Control.Exception    as E
import qualified Pacemaker as P
import qualified Database.Redis as R
import qualified KeyConfig as C
import qualified Feedback as F
import System.Environment


{--
curl -H "Content-type: application/json" -X POST \
    -d '{    
      "service_key": "e93facc04764012d7bfb002500d5d1a6",
      "incident_key": "srv01/HTTP",
      "event_type": "trigger",
      "description": "FAILURE for production/HTTP on machine srv01.acme.com",
      "client": "Sample Monitoring Service",
      "client_url": "https://monitoring.service.com",
      "details": {
        "ping time": "1500ms",
        "load avg": 0.75
      }
    }' \
    "https://events.pagerduty.com/generic/2010-04-15/create_event.json"

--}



data AlertMessage = 
  AlertMessage {
    org :: Text,
    check :: Text,
    tags :: Maybe [Text],
    anomaly_start :: Integer,
    state :: Text -- "OK" or "CRITICAL"
  } deriving (Show, Generic)

instance ToJSON AlertMessage



getEndpoint :: C.WebhookConfig -> Maybe Request
getEndpoint hook = parseUrl $ unpack (C.alert_endpoint hook) 
    
alert :: Request -> AlertMessage -> Maybe FeedbackAction -> IO ()
alert r ev feedback = do 
  result <- E.try $ sendAlert r ev :: IO (Either HttpException ())
  case result of  
    Left e -> print e
    _ -> applyFeedbackAfterSend feedback

  where
    sendAlert req body = do 
              withManager $ \manager -> do
                  let reqHead = req { 
                      method = "POST",
                      --checkStatus = \_ _  _ -> Nothing, -- lets let this fail and use this as error condition.
                      requestBody = RequestBodyBS (L.toStrict $ encode body),
                      responseTimeout = Just 10000000
                  }                    
                  _ <- http reqHead manager -- httpLbs for simpler fetching of body content                                        
                  return () --can also be responseHeaders or responseBody etc


-- we will talk to taut all the time. Just in case it wants to send it via a gateway.
tautAlert :: Integer -> Text -> R.Connection -> P.StoredHeartbeat -> IO ()
tautAlert currentTime entity conn shb = do 
    url <- lookupEnv "TAUT_URL"  
    conditionalAlert currentTime entity conn shb C.WebhookConfig {
      C.alert_endpoint =  pack $ fromMaybe "http://taut.radalert.io:8288" url,
      C.seconds_failing = Nothing
    }

maybeAlert :: Integer -> Text -> R.Connection -> P.StoredHeartbeat -> Maybe C.WebhookConfig -> IO ()
maybeAlert currentTime entity conn shb Nothing = 
  tautAlert currentTime entity conn shb
maybeAlert currentTime entity conn shb (Just conf) = 
  conditionalAlert currentTime entity conn shb conf
    

-- conditionally send an alert based on well a whole bunch of things. 
conditionalAlert :: Integer -> Text -> R.Connection -> P.StoredHeartbeat -> C.WebhookConfig -> IO ()    
conditionalAlert currentTime entity conn shb conf = do     
  feedback <- F.loadFeedback conn entity checkName
  if P.calculatedState shb == "CRITICAL" && F.alertingAllowed currentTime feedback
  then
    do  
      hs <- HS.listHistory conn entity checkName
      case isAlertable (catMaybes hs) (currentTime - (fromMaybe 120 $ C.seconds_failing conf)) of
        (True, howLongAgo) -> dispatchAlert conf (Just (FeedbackAction currentTime conn entity checkName feedback)) $ makeAlert shb howLongAgo entity "CRITICAL"--putStrLn $ "BE A LERT" ++ (show $ makeAlert shb duration entity)
        _ -> putStrLn "not yet ... sooon"      
  else  
      if P.calculatedState shb == "OK" then 
        do
          needToNotifyOK <- F.updateOK conn entity checkName feedback
          maybeSendOK needToNotifyOK conf $ makeAlert shb currentTime entity "OK"
      else return ()
  where
      checkName = P.check $ P.heartBeat shb      


maybeSendOK :: Bool -> C.WebhookConfig -> AlertMessage -> IO ()
maybeSendOK False _ _ = return ()
maybeSendOK True conf _alert = 
  dispatchAlert conf Nothing _alert
      
dispatchAlert :: C.WebhookConfig -> Maybe FeedbackAction -> AlertMessage -> IO ()
dispatchAlert whConf feedback _alert =  
  case getEndpoint whConf of
    Just ep -> 
      do 
        putStrLn $ "----> Dispatching webhook: " ++ (show _alert) ++ " for " ++ (show whConf)
        _ <- forkIO $ alert ep _alert feedback
        return ()
    Nothing -> 
        putStrLn $ "Not a valid endpoint for alerting " ++ (show whConf)        


data FeedbackAction = FeedbackAction Integer R.Connection C.Entity Text (Maybe F.AlertFeedback)
 
applyFeedbackAfterSend :: Maybe FeedbackAction -> IO ()
applyFeedbackAfterSend Nothing = return ()
applyFeedbackAfterSend (Just (FeedbackAction now conn entity checkName maybeFeedback)) = 
    F.storeFeedback conn entity checkName (F.alertSent now maybeFeedback)

makeAlert :: P.StoredHeartbeat -> Integer -> Text  -> Text -> AlertMessage
makeAlert shb howLongAgo entity _state = 
  AlertMessage {
    org = entity,
    check = P.check hb,
    tags = P.tags hb,
    anomaly_start = howLongAgo,
    state = _state
  }
  where
    hb = P.heartBeat shb

isAlertable :: [HS.HistoryMetric] -> Integer -> (Bool, Integer)
isAlertable hs howLongAgo = 
        case criticalList of 
          [] -> (False, 0)
          ls -> lastOf ls howLongAgo
        where
          criticalList = Prelude.takeWhile (\h' -> (HS.state h') == "CRITICAL") hs
            
lastOf :: [HS.HistoryMetric] -> Integer -> (Bool, Integer)
lastOf hs howLongAgo =  
  ((HS.timestamp theLast) < howLongAgo, HS.timestamp theLast)        
  --(True, HS.timestamp theLast)        
  where
      theLast = Prelude.last hs
  
