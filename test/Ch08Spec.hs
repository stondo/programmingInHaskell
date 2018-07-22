module Ch08Spec where

import Ch08

import Test.Hspec (Spec, describe, context, it, shouldBe)

import Data.Char

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch08 functions" $ do

   describe "isTaut p1" $ do
     it "should be False" $ do
       (isTaut p1) `shouldBe` False

   describe "isTaut p2" $ do
     it "should be True" $ do
       (isTaut p2) `shouldBe` True

   describe "isTaut p3" $ do
     it "should be False" $ do
       (isTaut p3) `shouldBe` False

   describe "isTaut p4" $ do
     it "should be False" $ do
       (isTaut p4) `shouldBe` True

   describe "value" $ do
     it "computes the operation of an expression on integers" $ do
       (value (Add (Add (Val 2) (Val 3)) (Val 4))) `shouldBe` 9

--    describe "" $ do
--      it "" $ do
--        () `shouldBe` 

--    describe "" $ do
--      it "" $ do
--        show () `shouldBe` ""
