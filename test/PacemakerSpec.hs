{-# LANGUAGE OverloadedStrings #-}

module PacemakerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Pacemaker as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Time calculations are the hardest problem in compute science" $ do
    it "Should pass sanity quick checks" $ property $
      \(seenT, expectedT, currentT) -> (currentT > seenT) ==> (P.adaptiveExpected seenT expectedT currentT) > currentT
    it "should apply default of 400seconds" $ do
      (P.calcExpiry (P.InboundHeart "api" "check" (Just "OK") Nothing Nothing Nothing Nothing Nothing Nothing Nothing) 2) `shouldBe` 402    
    it "should use supplied TTL" $ do
      (P.calcExpiry (P.InboundHeart "api" "check" (Just "OK") (Just 9) Nothing Nothing Nothing Nothing Nothing Nothing) 2) `shouldBe` 11
    it "should increase the effective TTL" $ do
      P.adaptiveExpected 1000 2000 4000 `shouldBe` 4400
    it "should increase the effective TTL below cap" $ do
      P.adaptiveExpected 100 200 210 `shouldBe` 342
    it "should keep effective TTL" $ do
      P.adaptiveExpected 100 200 199 `shouldBe` 299
    it "should reduce the effective TTL" $ do
      P.adaptiveExpected 100 200 101 `shouldBe` 191
    it "should cap the TTL calculation" $ do
      P.adaptiveExpected 1000 2000 (900000) `shouldBe` (900000 + 400) -- don't want it to grow past there

      
