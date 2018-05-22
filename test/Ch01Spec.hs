module Ch01Spec where

import Ch01

import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch01 functions" $ do

    describe "double" $ do
      it "doubles the int given as a parameter" $ do
        show (double 2) `shouldBe` "4"

    describe "recSum" $ do
      it "recursively sums all the numbers in a list" $ do
        show (recSum [1..10]) `shouldBe` "55"

    describe "qsort" $ do
      it "sorts a list of numbers using the quick sort algorithm" $ do
        show (qsort [7,1,19,11,0,2]) `shouldBe` "[0,1,2,7,11,19]"

-- Exercises
    describe "prod" $ do
      it "calculates the product of all numbers in a list" $ do
        show (prod [1..5]) `shouldBe` "120"

    describe "qsortReversed" $ do
      it "sorts a list of numbers descending using the quick sort algorithm" $ do
        show (qsortReversed [7,1,19,11,0,2]) `shouldBe` "[19,11,7,2,1,0]"

    describe "qsortWithoutDuplicates" $ do
      it "sorts a list of numbers, removing any duplicates, using the quick sort algorithm" $ do
        show (qsortWithoutDuplicates [1,9,1,19,9]) `shouldBe` "[1,9,19]"

    describe "qsortWithoutDuplicatesReversed" $ do
      it "sorts a list of numbers descending, removing any duplicates, using the quick sort algorithm" $ do
        show (qsortWithoutDuplicatesReversed [1,9,1,19,9]) `shouldBe` "[19,9,1]"
