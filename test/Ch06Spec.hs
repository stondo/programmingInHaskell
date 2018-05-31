module Ch06Spec where

import Ch06

import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch06 functions" $ do

    describe "fac" $ do
      it "calculates the factorial of the given integer using a recursive function" $ do
        show (fac 4) `shouldBe` "24"

    describe "insert" $ do
      it "inserts a new element of any ordered type into a sorted list to give another sorted list" $ do
        show (insert 3 [1,2,4,5]) `shouldBe` "[1,2,3,4,5]"

    describe "isort" $ do
      it "implements insertion sort using insert function to sort a list by inserting it's head by into the list that results from sorting its tail" $ do
        show (isort [3,2,1,4]) `shouldBe` "[1,2,3,4]"

    describe "fib" $ do
      it "calculates the nth Fibonacci number for any integer n >= 0 using double recursion" $ do
        show (fib 7) `shouldBe` "13"

    describe "evens" $ do
      it "selects the even elements from a list" $ do
        show (evens ['a', 'b', 'c', 'd', 'e']) `shouldBe` "\"ace\""

    describe "odds" $ do
      it "selects the odd elements from a list" $ do
        show (odds ['a', 'b', 'c', 'd', 'e']) `shouldBe` "\"bd\""

    describe "fac'" $ do
      it "calculates the factorial of the given integer using a recursive function" $ do
        show (fac' 5) `shouldBe` "120"

    describe "sumdown" $ do
      it "returns the sum of the non-negative integers from a given value" $ do
        show (sumdown 3) `shouldBe` "6"

    describe "powNonNegative" $ do
      it "exponentiation defined using recursion and the multiplication (*) operator 2^3" $ do
        show (powNonNegative 2 3) `shouldBe` "8"

    describe "powNonNegative" $ do
      it "exponentiation defined using recursion and the multiplication (*) operator 0^0" $ do
        show (powNonNegative 0 0) `shouldBe` "0"

    describe "powNonNegative" $ do
      it "exponentiation defined using recursion and the multiplication (*) operator 2^0" $ do
        show (powNonNegative 2 0) `shouldBe` "1"

    describe "euclid" $ do
      it "returns Greates Common Divisor" $ do
        show (euclid 6 27) `shouldBe` "3"


--    describe "" $ do
--      it "" $ do
--        show () `shouldBe` ""