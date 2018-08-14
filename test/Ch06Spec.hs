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

    describe "evensPos" $ do
      it "selects the even elements from a list" $ do
        show (evensPos ['a', 'b', 'c', 'd', 'e']) `shouldBe` "\"ace\""

    describe "oddsPos" $ do
      it "selects the odd elements from a list" $ do
        show (oddsPos ['a', 'b', 'c', 'd', 'e']) `shouldBe` "\"bd\""

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

    describe "and'" $ do
      it "decides if all logical values in a list are True" $ do
        show (and' [True,False,True,True]) `shouldBe` "False"

    describe "concat'" $ do
      it "concatenates a list of list" $ do
        show (concat' [[0,5],[1,3]]) `shouldBe` "[0,5,1,3]"

    describe "replicate'" $ do
      it "replicates n times the given value" $ do
        show (replicate' 3 True) `shouldBe` "[True,True,True]"

    describe "selNth" $ do
      it "selects the nth element of a list" $ do
        show (selNth [0,1,2,9] 3) `shouldBe` "9"

    describe "elem'" $ do
      it "decides if a value is an element of a list" $ do
        show (elem' 9 [1,9,2,0]) `shouldBe` "True"

    describe "merge" $ do
      it "merges two sorted lists" $ do
        show (merge [2,5,6] [1,3,4]) `shouldBe` "[1,2,3,4,5,6]"

    describe "msort" $ do
      it "merge sort using isort, halve and merge functions" $ do
        show (msort [9,10,1,2,7,6,5,8]) `shouldBe` "[1,2,5,6,7,8,9,10]"

    describe "sumList" $ do
      it "sums the numbers in a list" $ do
        show (sumList [0..10]) `shouldBe` "55"

    describe "takeRec" $ do
      it "takes n element from a list" $ do
        show (takeRec 3 [1..5]) `shouldBe` "[1,2,3]"

    describe "lastRec" $ do
      it "returns the last element of a non-empty list" $ do
        show (lastRec [1..9]) `shouldBe` "9"

--    describe "" $ do
--      it "" $ do
--        show () `shouldBe` ""

