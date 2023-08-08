module ListUtilsSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, shouldSatisfy, Spec )
import Test.QuickCheck ( (==>), Testable(property) )

import ListUtils (pick, rotate)

spec :: Spec
spec = do
    describe "pick" $ do
        it "picks subsets of correct size" $ property $
            \(n, xs) -> (n < 10 && length xs < 10) ==> (pick n xs :: [[Int]]) `shouldSatisfy` all ((==) n . length)
    describe "rotate" $ do
        it "rotates empty list" $
            rotate [] `shouldBe` ([] :: [Int])
        it "rotates singleton list" $
            rotate [1] `shouldBe` ([1] :: [Int])
        it "rotates long list" $
            rotate [1,2,3,4] `shouldBe` ([2,3,4,1] :: [Int])