module Ch10Spec where

import Ch10

import Test.Hspec (Spec, describe, context, it, shouldBe)


-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch10 functions" $ do

--    describe "" $ do
--      it "" $ do
--        () `shouldBe`

   describe "dummy test" $ do
     it "should pass" $ do
        "" `shouldBe` ""