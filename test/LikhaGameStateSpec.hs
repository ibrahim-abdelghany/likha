module LikhaGameStateSpec (spec) where

import Test.Hspec ( describe, it, Spec, shouldBe)
import Test.QuickCheck (Testable(property))
import Test.QuickCheck.Monadic (assert, monadicIO)

import Cards (Card(..), deck, Suit(..), Number(..), suit)
import LikhaGameState (generateFullGameState, ObservedGameState (PreGift, PostGift), FullGameState (..), PlayerState(..), missingSuits)
import LikhaGame (Player (..), players, Table (..))

import ArbitraryGameState(ArbitraryObservedPreGiftState(..), ArbitraryObservedPostGiftState(..))

import System.Random( newStdGen )
import Data.RVar (sampleStateRVar)
import Control.Monad.State (evalState)

spec :: Spec
spec = do
    describe "missingSuits" $ do
        it "returns nothing on empty history" $
            missingSuits Player0 []
                `shouldBe` []
        it "returns nothing on empty cards in Table in history" $
            missingSuits Player0 [Table Player1 []]
                `shouldBe` []
        it "returns player's missing suits from history" $
            missingSuits Player0 [Table Player1 [Card Clubs Ace, Card Clubs Two, Card Clubs Three, Card Hearts Seven]]
                `shouldBe` [Clubs]

    describe "sampleGameState" $ do
        it "samples pregift game state for observed hand" $ property $
            \(ArbitraryObservedPreGiftState starting p0cs) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateFullGameState (PreGift p0cs starting))) src
                assert $ case fullGameState of
                    FullPreGift p pss -> p == starting && playerStatesComplete pss && playerCardsPreserved Player0 p0cs pss
                    FullPostGift _ _ -> False
        it "preserves history for postgift game state" $ property $
            \(ArbitraryObservedPostGiftState p0cs p1cs history) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateFullGameState (PostGift p0cs p1cs history))) src
                assert $ case fullGameState of
                    FullPreGift _ _ -> False
                    FullPostGift _ hist -> hist == history
        it "preserves Player0 cards for postgift game state" $ property $
            \(ArbitraryObservedPostGiftState p0cs p1cs history) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateFullGameState (PostGift p0cs p1cs history))) src
                assert $ case fullGameState of
                    FullPreGift _ _ -> False
                    FullPostGift pss _ -> playerCardsPreserved Player0 p0cs pss
        it "generates complete cards for postgift state" $ property $
            \(ArbitraryObservedPostGiftState p0cs p1cs history) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateFullGameState (PostGift p0cs p1cs history))) src
                assert $ case fullGameState of
                    FullPreGift _ _ -> False
                    FullPostGift pss hist -> playerStatesAndHistoryComplete pss hist
        it "does not generate cards for missing suit in postgift state" $ property $
            \(ArbitraryObservedPostGiftState p0cs p1cs history) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateFullGameState (PostGift p0cs p1cs history))) src
                assert $ case fullGameState of
                    FullPreGift _ _ -> False
                    FullPostGift pss _ -> playerStatesLegal pss history

playerStatesLegal :: [PlayerState] -> [Table] -> Bool
playerStatesLegal pss history = all (`playerStateDoesntHaveIllegalSuits` history) pss

playerStateDoesntHaveIllegalSuits :: PlayerState -> [Table] -> Bool
playerStateDoesntHaveIllegalSuits (PlayerState p cs _) history = all ((`notElem` missing) . suit) cs
    where missing = missingSuits p history

playerStatesAndHistoryComplete :: [PlayerState] -> [Table] -> Bool
playerStatesAndHistoryComplete pss history = isPermutation allCards deck
    where allCards = concatMap hand pss ++ concatMap cards history

playerStatesComplete :: [PlayerState] -> Bool
playerStatesComplete pss = fullHands && allDeck && allPlayers
    where fullHands = all ((== 13) . length . hand) pss
          allDeck = isPermutation (concatMap hand pss) deck
          allPlayers = all ((`elem` players) . player) pss

playerCardsPreserved :: Player -> [Card] -> [PlayerState] -> Bool
playerCardsPreserved p pcs pss = isPermutation pHand pcs
    where pHand = hand $ head $ filter ((==) p . player) pss

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = isSubset xs ys  && isSubset ys xs

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs ys = all (`elem` ys) xs