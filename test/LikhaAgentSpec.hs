module LikhaAgentSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)
import Test.QuickCheck.Monadic (monadicIO)
import System.Random (newStdGen)
import LikhaAgent (runLikhaGame, randomLikhaAgent)
import Data.RVar (sampleStateRVar)
import Control.Monad.State (evalState)

spec :: Spec
spec = do
    describe "runLikhaGame" $ do
        it "runs a game with correct result" $ property $
            \() -> monadicIO $ do
                src <- newStdGen
                let likhaGame = runLikhaGame [randomLikhaAgent, randomLikhaAgent, randomLikhaAgent, randomLikhaAgent]
                let result = evalState (sampleStateRVar likhaGame) src
                return $ sum result == 36