{-# LANGUAGE InstanceSigs #-}
module LikhaGameStateSpec (spec) where

import Test.Hspec ( describe, it, Spec)
import Test.QuickCheck (Testable(property), Arbitrary (arbitrary), Gen, shuffle, oneof)
import Test.QuickCheck.Monadic (assert, monadicIO)

import Cards (Card, deck)
import LikhaGameState (generateFullGameState, ObservedGameState (PreGift), FullGameState (..), PlayerState(hand, player))
import LikhaGame (Player (..), players)

import System.Random( newStdGen )
import Data.RVar (sampleStateRVar)
import Control.Monad.State (evalState)

spec :: Spec
spec = do
    describe "sampleGameState" $ do
        it "samples game state for observed hand" $ property $
            \(PreGiftState starting p0cs) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateFullGameState (PreGift p0cs starting))) src
                assert $ case fullGameState of
                    FullPreGift p pss -> p == starting && playerStatesComplete pss && player0CardsPreserved p0cs pss
                    FullPostGift _ _ -> False

playerStatesComplete :: [PlayerState] -> Bool
playerStatesComplete pss = fullHands && allDeck && allPlayers
    where fullHands = all ((== 13) . length . hand) pss
          allDeck = all (all (`elem` deck) . hand) pss
          allPlayers = all ((`elem` players) . player) pss

player0CardsPreserved :: [Card] -> [PlayerState] -> Bool
player0CardsPreserved p0cs pss = all (`elem` p0Hand) p0cs &&  all (`elem` p0cs) p0Hand
    where p0Hand = hand $ head $ filter ((==) Player0 . player) pss

data PreGiftState = PreGiftState Player [Card]
    deriving Show

instance Arbitrary PreGiftState where
  arbitrary :: Gen PreGiftState
  arbitrary = do
    shuffledDeck <- shuffle deck
    starting <- oneof $ map return players
    let p0cs = take 13 shuffledDeck
    return $ PreGiftState starting p0cs