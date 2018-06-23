module Ch07Spec where

import Ch07

import Test.Hspec (Spec, describe, context, it, shouldBe)

import Data.Char

-- |Required for auto-discpvery
spec :: Spec
spec =
  describe "Ch07 functions" $ do

    describe "twice" $ do
      it "applies a function twice to the given argument" $ do
        show (twice (*2) 3) `shouldBe` "12"

    describe "sumsqreven" $ do
      it "calculates the sum of the squares of a list of integers (recursion)" $ do
        show (sumsqreven [1..10]) `shouldBe` "220"

    describe "sumsqreven'" $ do
      it "calculates the sum of the squares of a list of integers (map with explicit param)" $ do
        show (sumsqreven' [1..10]) `shouldBe` "220"

    describe "sumsqreven''" $ do
      it "calculates the sum of the squares of a list of integers (map with composition and implicit param)" $ do
        show (sumsqreven'' [1..10]) `shouldBe` "220"

    describe "bin2int and bin2int' " $ do
      it "convert a binary number in the form of an array and in reverse order to the corresponding integer" $ do
        show (bin2int [1,0,1,1] == bin2int' [1,0,1,1]) `shouldBe` "True"

    describe "int2bin" $ do
      it "converts and integer to its corresponding binary number (output to an array with inverted order)" $ do
        show (int2bin 13) `shouldBe` "[1,0,1,1]"

    describe "make8" $ do
      it "truncates or extends a binary number to make it precisely 8 bits" $ do
        show (make8 [1,0,1,1]) `shouldBe` "[1,0,1,1,0,0,0,0]"

    describe "encode" $ do
      it "encodes a string of characters in a list of concatenated 8 bits binary number corresponding to the Unicode number of each character" $ do
        show (encode "abc") `shouldBe` "[1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]"

    describe "chop8" $ do
      it "chops a list of bits into 8 bit binary number" $ do
        show (chop8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]) `shouldBe` "[[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]"

    describe "decode" $ do
      it "decodes a list of bits as a string" $ do
        show (decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]) `shouldBe` "\"abc\""

    describe "transmit" $ do
      it "simulates the transmisison of a string of characters as a list of bits using a perfect communication channel" $ do
        show (transmit "antani sbiricuda cum sblindosterno a destra!") `shouldBe` "\"antani sbiricuda cum sblindosterno a destra!\""

    describe "channel" $ do
      it "Bit type identity function" $ do
        show (channel [1,0,1,1,0,0,0,0]) `shouldBe` "[1,0,1,1,0,0,0,0]"

    describe "count" $ do
      it "counts the element in a list that are equals to the given one" $ do
        show (count "Red" votes) `shouldBe` "2"

    describe "rmdups" $ do
      it "removes duplicates from a list" $ do
        rmdups votes `shouldBe` ["Red", "Blue", "Green"]

    describe "result " $ do
      it "returns the result of a first-past-the-post election in increasing order of the number of votes received" $ do
        result votes `shouldBe` [(1,"Green"),(2,"Red"),(3,"Blue")]

    describe "winner" $ do
      it "is the last element in the result list" $ do
        winner votes `shouldBe` "Blue"

    describe "rmempty" $ do
      it "removes empty lists in a list of lists" $ do
        rmempty [[],[1,2,3],[],[],[],[1..10]] `shouldBe` [[1,2,3],[1,2,3,4,5,6,7,8,9,10]]

    describe "elim" $ do
      it "removes the given element from all of the lists in the list" $ do
        elim "Red" ballots `shouldBe` [["Green"],["Blue"],["Green","Blue"],["Blue","Gren"],["Green"]]

    describe "rank" $ do
      it "ranks the 1st-cjoice candidates in each ballot in increasing order of the number of such votes that were received" $ do
        rank ballots `shouldBe` ["Red","Blue","Green"]

    describe "winner'" $ do
      it "recursive function that implements the alternative vote algorithm" $ do
        winner' ballots `shouldBe` "Green"

    describe "Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using higher-order functions map and filter" $ do
      it "are indeed equal" $ do
        [(+1) x | x <- [0..10], even x] `shouldBe` map (+1) (filter (even) [0..10])

    describe "all'" $ do
      it "decides if all elements of a list satisfy a predicate" $ do
        all' even [2,4,6,8] `shouldBe` True

    describe "any'" $ do
      it "decides if any element of a list satisfies a predicate" $ do
          any' odd [0,2,1,3,6,8,9] `shouldBe` True

    describe "takeWhile'" $ do
      it "selects elemtnts from a list while they satisfy a predicate" $ do
        takeWhile' even [2,4,6,1,4,5] `shouldBe` [2,4,6]

    describe "takeWhile''" $ do
      it "selects elemtnts from a list while they satisfy a predicate" $ do
        takeWhile'' even [2,4,6,1,4,5] `shouldBe` [2,4,6]

    describe "dropWhile'" $ do
      it "removes elements from a list while they satisfy a predicate" $ do
        dropWhile' even [2,4,6,1,4,5] `shouldBe` [1,4,5]

    describe "map'" $ do
      it "applies the fiven function to every element of the list" $ do
        map' (+1) [1..5] `shouldBe` [2,3,4,5,6]

    describe "filter'" $ do
      it "filters the list with the given function" $ do
        filter' odd [0..10] `shouldBe` [1,3,5,7,9]

    describe "dec2int" $ do
      it "convers a decimal number (given in input as an array) into an integer" $ do
        dec2int [2,3,4,5] `shouldBe` 2345

    describe "int2binunfold" $ do
      it "converts and integer to its corresponding binary number (output to an array with inverted order)" $ do
        int2binUnfold 13 `shouldBe` [1,0,1,1]

    describe "chop8unfold" $ do
      it "chops a list of bits into 8 bit binary number" $ do
        chop8Unfold [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] `shouldBe` chop8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

    describe "mapUnfold" $ do
      it "applies the given function to every element of the list using unfold function" $ do
        mapUnfold (+1) [1..5] `shouldBe` [2,3,4,5,6]

    describe "iterateUnfold" $ do
      it "generates an infine list of the given value using unfold fucntion" $ do
        take 26 (iterateUnfold  (chr . (+1) . ord) 'a') `shouldBe` "abcdefghijklmnopqrstuvwxyz"

    describe "altMap" $ do
      it "alternately applies its two argument functions to successive elements in a list, in turn about order" $ do
        altMap (+10) (+100) [0..4] `shouldBe` [10,101,12,103,14]

    describe "altMap'" $ do
      it "alternately applies its two argument functions to successive elements in a list, in turn about order" $ do
        altMap' (+10) (+100) [0..4] `shouldBe` [10,101,12,103,14]

    describe "luhnAny" $ do
      it "decides if a bank card number of any length is valid" $ do
        luhnAny [1,7,8,4,1,7,8,4,1,7,8,4,1,7,8,4] `shouldBe` True

--    describe "" $ do
--      it "" $ do
--        show () `shouldBe` ""
