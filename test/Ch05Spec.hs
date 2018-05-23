module Ch05Spec where

import Ch05

import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch05 functions" $ do

    describe "concat'" $ do
      it "concatenates a list of lists" $ do
        show (concat' [[1, 2, 3], [99, 88, 45], [-1, -4, -9]]) `shouldBe` "[1,2,3,99,88,45,-1,-4,-9]"

    describe "firsts" $ do
      it "selects the first elements of each tuple in the list" $ do
        show (firsts [(1, 2), (99, 88), (-1, -4)]) `shouldBe` "[1,99,-1]"

    describe "length'" $ do
      it "returns the length of the list" $ do
        show (length' [1,2,3,99,88,45]) `shouldBe` "6"

    describe "factors'" $ do
      it "returns the factors of the given number" $ do
        show (factors 15) `shouldBe` "[1,3,5,15]"

    describe "prime" $ do
      it "check if a number is prime" $ do
        show (prime 7) `shouldBe` "True"

    describe "primes" $ do
      it "generates all prime number up to the given one" $ do
        show (primes 10) `shouldBe` "[2,3,5,7]"

    describe "find" $ do
      it "returns a list of all values associated with a given key in a table" $ do
        show (find 'a' [('a', 2),('c', 88),('a', -4)]) `shouldBe` "[2,-4]"

    describe "zip" $ do
      it "pairs successive elements frmo two existing lists" $ do
        show (['a', 'b', 'c'] `zip` [1,2,3,4]) `shouldBe` "[('a',1),('b',2),('c',3)]"

    describe "pairs" $ do
      it "returns the list of all pairs of adjacent elements from a list" $ do
        show (pairs [1,2,3,4]) `shouldBe` "[(1,2),(2,3),(3,4)]"

    describe "sorted" $ do
      it "decides if a list of elements of any ordered type is sorted" $ do
        show (sorted [1,2,3,4]) `shouldBe` "True"


--    describe "" $ do
--      it "" $ do
--        show () `shouldBe` ""