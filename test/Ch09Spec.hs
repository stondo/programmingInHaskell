module Ch09Spec where

import Ch09

import Test.Hspec (Spec, describe, context, it, shouldBe)

import Data.Char

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch09 functions" $ do

  describe "choicesListComp" $ do
    it "should compute all the possible choices given a list of positive integers." $ do
      (choicesListComp [1..4]) `shouldBe` choices [1..4]

--    describe "" $ do
--      it "" $ do
--        () `shouldBe`

--    describe "" $ do
--      it "" $ do
--        show () `shouldBe` ""
