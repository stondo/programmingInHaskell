module Ch02Spec where

import Ch01
import Ch02

import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch02 functions" $ do

    describe "quadruple" $ do
      it "quadruples the integer given as a parameter" $ do
        show (quadruple 2) `shouldBe` "8"

    describe "factorial" $ do
      it "calculates the factorial of the given int" $ do
        show (factorial 5) `shouldBe` "120"

    describe "average" $ do
      it "calculates the average of the int in a list" $ do
        show (average [1..8]) `shouldBe` "4"

-- Exercises
    describe "last'" $ do
      it "gets the last element in a list" $ do
        show (last [1..5]) `shouldBe` "5"

    describe "init'" $ do
      it "gets all elements ina list but last (version 1)" $ do
        show (init' [7,1,19,11,0,2]) `shouldBe` "[7,1,19,11,0]"

    describe "init''" $ do
      it "gets all elements ina list but last (version 2)" $ do
        show (init'' [1,9,1,19,9]) `shouldBe` "[1,9,1,19]"