{-# LANGUAGE InstanceSigs #-}
module ListUtilsSpec (spec) where

import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import Test.QuickCheck (Testable(property), Arbitrary, Gen)

import ListUtils (pick, rotate)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose)

spec :: Spec
spec = do
    describe "pick" $ do
        it "picks subsets of correct size" $ property $
            \(ArbitraryPickArguments n xs) -> (pick n xs :: [[Int]]) `shouldSatisfy` all ((==) n . length)
    describe "rotate" $ do
        it "rotates empty list" $
            rotate [] `shouldBe` ([] :: [Int])
        it "rotates singleton list" $
            rotate [1] `shouldBe` ([1] :: [Int])
        it "rotates long list" $
            rotate [1,2,3,4] `shouldBe` ([2,3,4,1] :: [Int])

data ArbitraryPickArguments = ArbitraryPickArguments Int [Int]
    deriving (Show)

instance Arbitrary ArbitraryPickArguments where
  arbitrary :: Gen ArbitraryPickArguments
  arbitrary = do
    n <- choose (0, 9)
    m <- choose (0, 9)
    let list = take m [1..]
    return $ ArbitraryPickArguments n list

