{-# LANGUAGE OverloadedStrings, DeriveGeneric, LambdaCase #-}
module Metrics where

import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock.POSIX
import Data.Maybe
import System.Environment
import Network.HTTP.Base (urlEncode)
import qualified Control.Exception    as E




-- this is what is send to learning engine
data LearnRequest = 
  LearnRequest {
    check_id :: Text, 
    time :: Integer, 
    _value :: Double    
  } deriving (Show, Generic)

instance ToJSON LearnRequest where
    toJSON (lr) =
      object [ 
          "check_id"     .= (check_id lr)
        , "time"         .= (time lr)
        , "value"        .= (_value lr)
      ]
            
-- this is what comes back from the learning engine
data LearnResponse = 
  LearnResponse {
    anomalous :: Bool, 
    likelihood :: Double, 
    anomalyScore :: Double, 
    predicted :: Maybe Double,
    current_value :: Double
  } deriving (Show, Generic)

instance FromJSON LearnResponse

    
toLearnReq :: Integer -> MetricValue -> CheckID -> LearnRequest
toLearnReq currentTime mValue chkId = 
  LearnRequest new_check_id currentTime mValue  
  where new_check_id = cleanse chkId

calculatedState :: Maybe LearnResponse -> Text
calculatedState = \case 
                      Nothing -> "UNKNOWN"
                      Just lr -> toCritical . anomalous $ lr
  


    
toCritical :: Bool -> Text
toCritical b = 
    if b
    then "CRITICAL"
    else "OK"

-- call out to perform learning
performLearn :: Request -> LearnRequest -> IO (Maybe LearnResponse)
performLearn req body = do 
          withManager $ \manager -> do
              let reqHead = req { 
                  method = "POST",
                  checkStatus = \_ _  _ -> Nothing,  -- stops it barfing on non 200OK FFS.
                  requestBody = RequestBodyBS (L.toStrict $ encode body)
              }                    
              res <- httpLbs reqHead manager -- httpLbs for simpler fetching of body content                                        
              return $ decode (responseBody res)

doLearn :: Request -> LearnRequest -> IO (Maybe LearnResponse)
doLearn req body = do
  result <- E.try (performLearn req body) :: IO (Either HttpException (Maybe LearnResponse))
  case result of
    Left er -> 
      do
        print er
        putStrLn "Unable to learn, learning service may be unavailable"
        return Nothing
    Right v -> return v    
        
{--

ld :: IO (Maybe APIKeyMap)
ld = do
  url <- env "CONSOLE_URL" "http://localhost:6789/api_keys.json"
  auth <- env "CONSOLE_AUTH" "rad"
  result <- E.try (fetchC url auth) :: IO (Either HttpException (Maybe APIKeyMap))
  case result of 
    Left e -> 
      do 
        print e
        return Nothing
    Right v -> return v


--}
            
          
        
getLearnService :: IO Request
getLearnService = do
  url <- lookupEnv "LEARN_URL"  
  case parseUrl (fromMaybe "http://neocortex:8080" url) of 
    Nothing -> error "Not a valid learning service url"
    Just req -> return req  
     

cleanse :: Text -> Text
cleanse t = replace "%" "" $ pack $ urlEncode $ unpack t




      


type MetricName = Text
type MetricValue = Double
type CheckID = Text

currentTimeS :: IO Integer
currentTimeS = round `fmap` getPOSIXTime 
