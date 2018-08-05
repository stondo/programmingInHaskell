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

   describe "valueSimple" $ do
     it "computes the result of an expression on integers" $ do
       (valueSimple (Add (Add (Val 2) (Val 3)) (Val 4))) `shouldBe` 9

   describe "value" $ do
     it "computes the result of an expression on integers using recursive structures" $ do
       (value (Add (Add (Val 2) (Val 3)) (Val 4))) `shouldBe` (valueSimple (Add (Add (Val 2) (Val 3)) (Val 4)))

   describe "multNat" $ do
     it "should compute the multiplication between 2 NAT" $ do
       show (multNat (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero)))) `shouldBe` "Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))"

   describe "multNat" $ do
     it "should compute the multiplication between 2 NAT" $ do
       show (multNat (Succ (Succ (Succ Zero))) Zero) `shouldBe` "Zero"

   describe "multNat" $ do
     it "should compute the multiplication between 2 NAT" $ do
       show (multNat Zero (Succ (Succ (Succ Zero)))) `shouldBe` "Zero"

   describe "multNat" $ do
     it "should compute the multiplication between 2 NAT" $ do
       show (multNat (Succ Zero) (Succ (Succ Zero))) `shouldBe` "Succ (Succ Zero)"

   describe "multNat" $ do
     it "should compute the multiplication between 2 NAT" $ do
       show (multNat (Succ (Succ Zero)) (Succ Zero)) `shouldBe` "Succ (Succ Zero)"

--    describe "" $ do
--      it "" $ do
--        () `shouldBe` 

--    describe "" $ do
--      it "" $ do
--        show () `shouldBe` ""
