{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Feedback where

import Data.Aeson
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics
import qualified Database.Redis as R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified KeyConfig as C
import Data.Time.Clock.POSIX


data AlertFeedback = 
  AlertFeedback {
    currentQuiet :: Double,
    alertAfter :: Integer,
    inEvent :: Bool  
  } deriving (Show, Generic, Eq)

instance ToJSON AlertFeedback 
instance FromJSON AlertFeedback 

data Vote = 
    Vote {
      api_key :: T.Text, 
      vote :: T.Text, 
      check :: T.Text
    } deriving (Show, Generic, Eq)

instance FromJSON Vote      


data FeedbackRating = Up | Down  

-- in hours
defaultQuiet :: Double
defaultQuiet = 1.0  

minQuiet :: Double
minQuiet = 1.0

maxQuiet :: Double
maxQuiet = 48.0


loadFeedback :: R.Connection -> C.Entity -> T.Text -> IO (Maybe AlertFeedback)
loadFeedback conn entity checkName = do
    res <- R.runRedis conn $ R.get (feedKey entity checkName)
    case res of 
      Right (Just af) -> return $ toFeed af
      _ -> return Nothing
    where
      toFeed = decode . B.fromStrict 
   

storeFeedback :: R.Connection -> C.Entity -> T.Text -> AlertFeedback -> IO ()
storeFeedback conn entity checkName feedback = do
  putStrLn $ "Storing feedback " ++ (show feedback)
  res <- R.runRedis conn $ R.set (feedKey entity checkName) (B.toStrict (encode feedback))
  case res of 
    Right _ -> return ()
    _ -> do
      putStrLn "Problems storing metric"

feedKey :: C.Entity -> T.Text -> BS.ByteString
feedKey entity _check = encodeUtf8 $ T.concat ["feedback", ":", entity, ":", _check]


storeVote :: R.Connection -> C.Entity -> Vote -> FeedbackRating -> IO () 
storeVote conn entity v fb = do
  stored <- loadFeedback conn entity checkName
  now <- currentTimeS
  case stored of 
    Just s -> storeFeedback conn entity checkName (takeFeedback now s fb)
    Nothing -> putStrLn $ "IGNORED: No vote was taken as no alert feedback data yet: " ++ (show v)  
  where
      checkName = check v
      

-- store the fact it is ok now. TODO: decay the currentQuiet value very slowly over time here.
updateOK :: R.Connection -> C.Entity -> T.Text -> Maybe AlertFeedback -> IO Bool
updateOK _ _ _ Nothing = return False
updateOK conn entity checkName (Just feedback) =  
  if up /= feedback 
  then
    do 
      storeFeedback conn entity checkName up
      return True
  else 
    return False
  where
      up = noLongerCritical feedback

checkVote :: Vote -> Maybe FeedbackRating
checkVote v = 
  case (vote v) of 
    "up" -> Just Up
    "down" -> Just Down 
    _ -> Nothing

-- apply the feedback and calculate the next alert time and the score  
takeFeedback :: Integer -> AlertFeedback -> FeedbackRating -> AlertFeedback  

takeFeedback timeNow current Down = 
  current {
    currentQuiet = newQuiet,
    alertAfter = nextAlert
  }
  where
    newQuiet = min maxQuiet $ (currentQuiet current) * 2.0
    nextAlert =  timeNow + max (toSeconds newQuiet) (toSeconds 5) -- at least 5 hours to shut up
    
takeFeedback timeNow current Up = 
  current {
    currentQuiet = newQuiet,
    alertAfter = nextAlert 
  }
  where
    newQuiet = max minQuiet $ (currentQuiet current) * 0.6
    nextAlert = timeNow + max (toSeconds newQuiet) (toSeconds 3) -- at least 3 hours to shut up

-- update the feedback record after the alert has been sent
alertSent :: Integer -> Maybe AlertFeedback -> AlertFeedback
alertSent now Nothing = 
  AlertFeedback { 
    currentQuiet = defaultQuiet,
    alertAfter = nextAlert,
    inEvent = True
  }
  where
    nextAlert = now + (toSeconds defaultQuiet)
    
alertSent now (Just current) = 
    current {
      inEvent = True,
      alertAfter = nextAlert
    }
    where
      nextAlert = now + (toSeconds (currentQuiet current))


alertingAllowed :: Integer -> Maybe AlertFeedback -> Bool
alertingAllowed _ Nothing = True
alertingAllowed now (Just al) = 
   now >= (alertAfter al) && not (inEvent al)
      


-- no longer need to track that it is critical 
noLongerCritical :: AlertFeedback -> AlertFeedback
noLongerCritical al = al { inEvent = False }  

-- convert hours to seconds
toSeconds :: Double -> Integer
toSeconds t = round $ 3600 * t


currentTimeS :: IO Integer
currentTimeS = round `fmap` getPOSIXTime 

  
  
