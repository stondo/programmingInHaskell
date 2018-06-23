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

    describe "addLambda" $ do
      it "sums 2 integers using lambda function" $ do
        show (addLambda 3 4) `shouldBe` "7"

    describe "odds" $ do
      it "generates n odds number" $ do
        show (odds 11) `shouldBe` "[1,3,5,7,9,11,13,15,17,19,21]"

    describe "odds'" $ do
      it "generates n odds number" $ do
        show (odds' 13) `shouldBe` "[1,3,5,7,9,11,13,15,17,19,21,23,25]"

    describe "sumFoldl" $ do
      it "sums all of the integers in a list using fold left" $ do
        show (sumFoldl [0..10]) `shouldBe` "55"

    describe "halve" $ do
      it "splits in 2 a list containing an even number of elements" $ do
        show (halve [1..10]) `shouldBe` "([1,2,3,4,5],[6,7,8,9,10])"

    describe "third" $ do
      it "selects the 3rd element in a list using index operator (!!)" $ do
        show (third [1..10]) `shouldBe` "3"

    describe "third'" $ do
      it "selects the 3rd element in a list using head/tail" $ do
        show (third' [1..10]) `shouldBe` "3"

    describe "third''" $ do
      it "selects the 3rd element in a list using pattern matching" $ do
        show (third'' [1..10]) `shouldBe` "3"

    describe "safetail" $ do
      it "safely selects the tail of a list" $ do
        show (safetail [1..4]) `shouldBe` "[2,3,4]"

    describe "safetail'" $ do
      it "safely selects the tail of a list" $ do
        show (safetail' [1..4]) `shouldBe` "[2,3,4]"

    describe "safetail''" $ do
      it "safely selects the tail of a list" $ do
        show (safetail'' [1..4]) `shouldBe` "[2,3,4]"

    describe "multLambda" $ do
      it "multiplies the 3 integers using lamba function" $ do
        show (multLambda 4 5 3) `shouldBe` "60"

    describe "luhnDouble" $ do
      it "double the integer if <= 9 else it doubles it and subtract 9 using guards" $ do
        show (luhnDouble 3) `shouldBe` "6"

    describe "luhnDouble" $ do
      it "double the integer if <= 9 else it doubles it and subtract 9 using if/else" $ do
        show (luhnDouble 6) `shouldBe` "3"

    describe "luhn" $ do
      it "decides if a four-digit bank card number is valid (VALID)" $ do
        show (luhn 1 7 8 4) `shouldBe` "True"

    describe "luhn" $ do
      it "decides if a four-digit bank card number is valid (NOT VALID)" $ do
        show (luhn 5 7 2 9) `shouldBe` "False"


