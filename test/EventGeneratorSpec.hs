{-# LANGUAGE OverloadedStrings #-}

module EventGeneratorSpec (main, spec) where

import Test.Hspec
import qualified EventGenerator as E
import qualified Pacemaker as P

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

hb :: P.InboundHeart 
hb = P.InboundHeart "api" "check" (Just "OK") (Just 1000) Nothing Nothing Nothing Nothing Nothing Nothing

hbNoExpire :: P.InboundHeart 
hbNoExpire = P.InboundHeart "api" "check" (Just "OK") (Just (-1)) Nothing Nothing Nothing Nothing Nothing Nothing

hbExpiringNext :: P.InboundHeart 
hbExpiringNext = P.InboundHeart "api" "check" (Just "CRITICAL") (Just 1) Nothing Nothing Nothing Nothing (Just "OK") Nothing

expiringSHB :: P.StoredHeartbeat
expiringSHB = P.StoredHeartbeat {
  P.heartBeat = hb,
  P.expires = 2,
  P.lastSeen = 1,
  P.calculatedState = "OK"
}      



spec :: Spec
spec = do
  describe "Check event generation" $ do
    it "should not expire if negative" $ do
      (E.canExpire hbNoExpire) `shouldBe` False
    it "should expire" $ do
      (E.canExpire hbExpiringNext) `shouldBe` True
    it "should be UNKNOWN" $ do
      (E.calcState expiringSHB (E.CurrentTime 24)) `shouldBe` ("UNKNOWN", Nothing)
    it "should timeout to OK" $ do
      (E.calcState expiringSHB { P.heartBeat = hbExpiringNext } (E.CurrentTime 24)) `shouldBe` ("OK", Nothing)
    it "should not have expired yet" $ do
      (E.calcState expiringSHB (E.CurrentTime 1)) `shouldBe` ("OK", Nothing)
    it "should never expire" $ do
      (E.calcState expiringSHB { P.heartBeat = hbNoExpire } (E.CurrentTime 100)) `shouldBe` ("OK", Nothing)

      
          
          
