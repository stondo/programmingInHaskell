module Ch05Spec where

import Ch05

import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch05 functions" $ do

    describe "concat'" $ do
      it "concatenates a list of lists" $ do
        show (concat' [[1,2,3],[99,88,45],[-1,-4,-9]]) `shouldBe` "[1,2,3,99,88,45,-1,-4,-9]"


-- firsts
-- length'
-- factors
-- prime
-- primes
-- find

