{-# LANGUAGE OverloadedStrings #-}

module WebServer where

import Control.Monad.IO.Class (liftIO)
import qualified Database.Redis as R
import Web.Scotty
import Network.Wai                          (Request (..), requestHeaders)
import qualified Pacemaker as P
import qualified KeyConfig as C
import Network.HTTP.Types
import qualified Data.Text as T
import qualified EventGenerator as E
import qualified HistoricalData as HD
import qualified Feedback as F
import Data.IORef



main :: P.PaceRedis -> IORef C.APIKeyMap -> IO ()
main (P.PaceRedis paceRedis) api_keys = scotty 7223 $ do
  rConn <- liftIO $ R.connect paceRedis  
  -- _ <- liftIO $ putStrLn (show api_keys)

  get "/" $ do
    text "OK"

  post "/http" $ do
    j <- jsonData
    storeHeart api_keys rConn (makeHttp j)

  post "/vote" $ do
    j <- jsonData
    takeVote api_keys rConn j

  post "/" $ do
    j <- jsonData
    storeHeart api_keys rConn j

  -- get .. ?api_key=22321432&type=heartbeat|http
  get "/heartbeats" $ fetchHeartBeats api_keys rConn

  -- delete ..?api_key=...&check=foo.com&type=heartbeat|http
  delete "/heartbeats" $ deleteHeartbeats api_keys rConn  
  
  get "/history" $ fetchHistory api_keys rConn 

    
  post "/echo" $ do
    r <- request
    ct <- header "Authorization"    
    b <- body
    _ <- liftIO $ putStrLn (show b)
    _ <- liftIO $ putStrLn (show ct)
    _ <- liftIO $ putStrLn (show (requestHeaders r))
    text "ok"

makeHttp :: P.InboundHeart -> P.InboundHeart
makeHttp h = h { P.heartbeat_type = Just "http" }


takeVote :: IORef C.APIKeyMap -> R.Connection -> F.Vote -> ActionM ()
takeVote api_keys rConn v = do 
  case F.checkVote v of 
    Just fb -> withAPIKey (F.api_key v) api_keys (\entity -> liftIO (F.storeVote rConn entity v fb) >>= json)
    _ -> status status400 >> text "Not a valid vote value - should be 'up' or 'down'."
  
       

storeHeart :: IORef C.APIKeyMap -> R.Connection -> P.InboundHeart -> ActionM ()
storeHeart api_keys rConn j = do
  if (P.checkState j) 
  then
    withAPIKey (P.api_key j) api_keys (\entity -> liftIO (P.pingbotBeat entity rConn j) >>= json)
  else 
    status status400 >> text "Not a valid state value - should be OK or CRITICAL - or you should include a metric number to learn from."

-- validates the api key as legit before taking action
withAPIKey :: T.Text -> IORef C.APIKeyMap -> (T.Text -> ActionM ()) -> ActionM ()
withAPIKey apiKey keyMapR action = do
  keyMap <- liftIO $ readIORef keyMapR
  case C.checkKey apiKey keyMap of 
    C.KeyOK entity -> action entity
    C.InvalidKey -> do 
      status status401 
      text "Unknown api_key - check key or try again shortly."
    
fetchHeartBeats :: IORef C.APIKeyMap -> R.Connection -> ActionM()
fetchHeartBeats keyMap rConn = do
    apiKey <- param "api_key"
    --beatType <- param "type" 
    withAPIKey apiKey keyMap (\entity -> liftIO (E.getHeartbeats (E.PaceConn rConn) entity heartbeatType) >>= json)
    
fetchHistory :: IORef C.APIKeyMap -> R.Connection -> ActionM()
fetchHistory keyMap rConn = do
    apiKey <- param "api_key"
    checkName <- param "check"
    withAPIKey apiKey keyMap (\entity -> liftIO (HD.listHistory rConn entity checkName) >>= json)  

deleteHeartbeats :: IORef C.APIKeyMap -> R.Connection -> ActionM()
deleteHeartbeats keyMap conn = do
  apiKey <- param "api_key"
  checkName <- param "check"
  --heartbeatType <- param "type"
  withAPIKey apiKey keyMap (\entity -> liftIO (P.removeCheck conn entity checkName heartbeatType))
  text "OK"

heartbeatType :: T.Text
heartbeatType = "heartbeat"
