{-# LANGUAGE OverloadedStrings #-}

module FeedbackSpec (main, spec) where

import Test.Hspec
import Feedback
import Test.QuickCheck
 
main :: IO ()
main = hspec spec



spec :: Spec
spec = do
  describe "Feedback handling" $ do
    it "should convert hours to seconds" $ do
      (toSeconds 1.0) `shouldBe` 3600
    it "should be not in event"   $ do
      noLongerCritical AlertFeedback {currentQuiet = 1.0, alertAfter = 1234, inEvent = True} `shouldBe` AlertFeedback {inEvent = False, currentQuiet = 1.0, alertAfter = 1234}
    it "should set default after sending" $ property $
      \now ->  (alertSent now Nothing)  ==  AlertFeedback { inEvent = True, currentQuiet = defaultQuiet, alertAfter = ( now + toSeconds defaultQuiet) } 
    it "should alwas be after now" $ property $
      \now -> (now > 0) ==> alertAfter (alertSent now Nothing)   > now
    it "should always be inEvent" $ property $
      \(now, ev, af, q) -> inEvent (alertSent now $ Just AlertFeedback { inEvent = ev, alertAfter = af, currentQuiet = q}) == True
    it "should always be after now existing" $ property $
      \(now, ev, af, q) -> (now > 0 && q > 0) ==> alertAfter (alertSent now $ Just AlertFeedback { inEvent = ev, alertAfter = af, currentQuiet = q}) >  now    
    it "should always convert to seconds" $ property $
      \(now, ev, af, q) -> (now > 0 && q > 0) ==> alertAfter (alertSent now $ Just AlertFeedback { inEvent = ev, alertAfter = af, currentQuiet = q}) >  toSeconds q    
    it "should not change quiet" $ property $
      \(now, ev, af, q) -> (now > 0 && q > 0) ==> currentQuiet (alertSent now $ Just AlertFeedback { inEvent = ev, alertAfter = af, currentQuiet = q}) == q      
    it "Down should increase quiet" $ property $
      \(now,ev,q,al) -> (q > 0 && q < 48) ==> currentQuiet (takeFeedback now AlertFeedback { inEvent = ev, currentQuiet = q, alertAfter = al} Down) >= q
    it "Down should not increase past 48" $ property $
      \(now,ev,q,al) -> (q > 0) ==> currentQuiet (takeFeedback now AlertFeedback { inEvent = ev, currentQuiet = q, alertAfter = al} Down) <= 48.0
    it "Down should ack at least 5 hours" $ property $
      \(now,ev,q,al) -> (q > 0 && now > 0 && al > 0) ==> alertAfter (takeFeedback now AlertFeedback { inEvent = ev, currentQuiet = q, alertAfter = al} Down) >= now + (toSeconds 5)
    it "Down always alert after now" $ do
      alertAfter (takeFeedback 1200 AlertFeedback { inEvent = True, currentQuiet = 5.0, alertAfter = 1000} Down) > 1200 `shouldBe` True      
    it "Down should double quiet time for next time" $ do
      currentQuiet (takeFeedback 12 AlertFeedback { inEvent = False, currentQuiet = 5.0, alertAfter = 1} Down) == 10.0
    it "Up should reduce quiet" $ property $ 
      \(now,ev,q,al) -> (q > 3 && q < 48) ==> currentQuiet (takeFeedback now AlertFeedback { inEvent = ev, currentQuiet = q, alertAfter = al} Up) < q
    it "Up should not reduce below 1 hours" $ property $ 
      \(now,ev,q,al) -> (q > 0) ==> currentQuiet (takeFeedback now AlertFeedback { inEvent = ev, currentQuiet = q, alertAfter = al} Up) >= 1.0
    it "Up should ack at least 3 hours" $ property $
      \(now,ev,q,al) -> (q > 0 && now > 0 && al > 0) ==> alertAfter (takeFeedback now AlertFeedback { inEvent = ev, currentQuiet = q, alertAfter = al} Up) >= now + (toSeconds 3)
    it "Up should always alert after now" $ do
      alertAfter (takeFeedback 1200 AlertFeedback { inEvent = True, currentQuiet = 5.0, alertAfter = 1000} Up) > 1200 `shouldBe` True      
    it "Up should reduce quiet time for next time" $ do
      currentQuiet (takeFeedback 12 AlertFeedback { inEvent = False, currentQuiet = 10.0, alertAfter = 1} Up) == 6.0

    it "Up should simply shut up for a while" $ do
      alertAfter (takeFeedback 12 AlertFeedback { inEvent = False, currentQuiet = 10.0, alertAfter = 1} Up) > 12 + 3600
    it "Down should simply shut up for a while" $ do
      alertAfter (takeFeedback 12 AlertFeedback { inEvent = False, currentQuiet = 10.0, alertAfter = 1} Down) > 12 + 3600
    it "Should always shut up for a while after sending" $ do
      alertAfter (alertSent 12 $ Just AlertFeedback { inEvent = False, alertAfter = 10, currentQuiet = 10.0}) > 12 + 3600
    it "Should allowAlerting" $ do
      alertingAllowed 42 Nothing `shouldBe` True
    it "Should not allow alerting" $ do
      alertingAllowed 42 (Just $ AlertFeedback { inEvent=False, alertAfter = 43, currentQuiet = 1}) `shouldBe` False 
    it "Should allow alerting" $ do
      alertingAllowed 42 (Just $ AlertFeedback { inEvent=False, alertAfter = 40, currentQuiet = 1}) `shouldBe` True 
    it "Should not allow alerting" $ do
      alertingAllowed 42 (Just $ AlertFeedback { inEvent=True, alertAfter = 40, currentQuiet = 1}) `shouldBe` False 
