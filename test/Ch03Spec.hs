module Ch03Spec where

import Ch03

import Test.Hspec (Spec, describe, context, it, shouldBe)

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch03 functions" $ do
    describe "add" $ do

      it "adds 2 int" $ do
        show (add(2, 6)) `shouldBe` "8"

    describe "zeroto" $ do

      it "generates a list of intsg up to the given int" $ do
        show (zeroto 5) `shouldBe` "[0,1,2,3,4,5]"

    describe "add'" $ do

      it "adds 2 int (curried)" $ do
        show (add' 2 2) `shouldBe` "4"

    describe "mult" $ do

      it "multiplies the 3 int given as params" $ do
        show (mult 4 5 3) `shouldBe` "60"