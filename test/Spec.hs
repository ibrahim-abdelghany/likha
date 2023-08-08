import Test.Hspec ( hspec, describe, it, shouldSatisfy )
import Test.QuickCheck ( (==>), Testable(property) )

import ListUtils (pick)

main :: IO ()
main = hspec $ do
    describe "pick" $ do
        it "picks subsets of correct size" $ property $
            \(n, xs) -> (n < 10 && length xs < 10) ==> (pick n xs :: [[Int]]) `shouldSatisfy` all ((==) n . length)
    

