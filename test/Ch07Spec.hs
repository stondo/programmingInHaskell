module Ch07Spec where

import Ch07

import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch07 functions" $ do

    describe "twice" $ do
      it "applies a function twice to the given argument" $ do
        show (twice (*2) 3) `shouldBe` "12"

    describe "sumsqreven" $ do
      it "calculates the sum of the squares of a list of integers (recursion)" $ do
        show (sumsqreven [1..10]) `shouldBe` "220"

    describe "sumsqreven'" $ do
      it "calculates the sum of the squares of a list of integers (map with explicit param)" $ do
        show (sumsqreven' [1..10]) `shouldBe` "220"

    describe "sumsqreven''" $ do
      it "calculates the sum of the squares of a list of integers (map with composition and implicit param)" $ do
        show (sumsqreven'' [1..10]) `shouldBe` "220"


--    describe "" $ do
--      it "" $ do
--        show () `shouldBe` ""