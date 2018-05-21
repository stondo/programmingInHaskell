module Ch04Spec where

import Ch04

import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch04 functions" $ do
    describe "even'" $ do

      it "decide if an integer is even" $ do
        show (even' 6) `shouldBe` "True"

    describe "splitAt'" $ do

      it "splits a list at the nth element" $ do
        show (splitAt' 4 [0,1,2,3,4,5]) `shouldBe` "([0,1,2,3],[4,5])"

    describe "recip'" $ do

      it "reciprocation" $ do
        show (recip' 4) `shouldBe` "0.25"

    describe "signum'" $ do

      it "returns the sign of an integer" $ do
        show (signum'(-9)) `shouldBe` "-1"

    describe "fst'" $ do

      it "selects the first element of a tuple" $ do
        show (fst' (4, 9)) `shouldBe` "4"

    describe "snd'" $ do

      it "selects the second element of a tuple" $ do
        show (snd' (4, 9)) `shouldBe` "9"

    describe "abs'" $ do

      it "returns the absolute value of the given number" $ do
        show (abs'(-8)) `shouldBe` "8"

    describe "abs' and abs''" $ do

      it "abs' should be equal to abs'' for any number with opposite sign" $ do
        abs'(-7) `shouldBe` abs'' 7

    describe "abs''" $ do

      it "returns the absolute value of the given number" $ do
        show (abs'' 8) `shouldBe` "8"
      
    describe "multLambda" $ do

      it "multiplies the 3 int given as params using lambda notation" $ do
        show (multLambda 4 5 2) `shouldBe` "40"

    describe "signum'" $ do

      it "returns the sign of an integer" $ do
        show (signum' 9) `shouldBe` "1"



--  addLambda
--  odds
--  odds'
--  sumFoldl
--  halve
--  third
--  third'
--  third''
--  safetail
--  safetail'
--  safetail''
--  mult
--  luhnDouble
--  luhnDouble'
--  luhn

