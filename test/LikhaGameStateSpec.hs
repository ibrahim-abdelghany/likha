{-# LANGUAGE InstanceSigs #-}
module LikhaGameStateSpec (spec) where

import Test.Hspec ( describe, it, Spec, shouldBe, shouldSatisfy)
import Test.QuickCheck (Testable(property), Arbitrary (arbitrary), oneof)
import Test.QuickCheck.Monadic (assert, monadicIO)

import Cards (Card(..), deck, Suit(..), Number(..), suit)
import LikhaGameState
    ( MoveOptions(..),
      moveOptions,
      generateRandomFullGameState,
      turn,
      ObservedGameState(PreGift, PostGift),
      FullGameState(..),
      PlayerState(..),
      missingSuits,
      Move(..),
      applyMove,
      observeFullGameState,
      observePlayer )
import LikhaGame (Player (..), players, Table (..))

import ArbitraryGameState(ArbitraryFullGameState(..), ArbitraryObservedPreGiftState(..), ArbitraryObservedPostGiftState(..), ArbitraryStartFullPostGiftState (..))

import System.Random( newStdGen )
import Data.RVar (sampleStateRVar)
import Control.Monad.State (evalState)
import Test.QuickCheck.Gen (Gen)
import Data.List (sortOn)
import ListUtils (rotate)

spec :: Spec
spec = do
    describe "moveOptions" $ do
        it "returns valid options" $ property $
            \(ArbitraryFullGameState fgs) -> moveOptions fgs `shouldSatisfy` movesLegal fgs

    describe "applyMove" $ do
        it "updates state correctly" $ property $
            \(ArbitraryStateMove fgs move) -> applyMove move fgs `shouldSatisfy` applyMoveWorks move fgs

    describe "observePlayer" $ do
        it "Player0 observes player correctly" $ 
            map (observePlayer Player0) players `shouldBe` players
        it "Player1 observes player correctly" $ 
            map (observePlayer Player1) players `shouldBe` [Player3, Player0, Player1, Player2]
        it "Player2 observes player correctly" $ 
            map (observePlayer Player2) players `shouldBe` [Player2, Player3, Player0, Player1]
        it "Player3 observes player correctly" $ 
            map (observePlayer Player3) players `shouldBe` [Player1, Player2, Player3, Player0]

    describe "observeFullGameState" $ do
        it "observes state correctly" $ property $
            \(ArbitraryStartFullPostGiftState start current) -> observeFullGameState Player0 start current `shouldSatisfy` correctlyObserved Player0 start current

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

    describe "generateRandomFullGameState" $ do
        it "samples pregift game state for observed hand" $ property $
            \(ArbitraryObservedPreGiftState starting p0cs) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateRandomFullGameState (PreGift p0cs starting))) src
                assert $ case fullGameState of
                    FullPreGift p pss -> p == starting && playerStatesComplete pss && playerCardsPreserved Player0 p0cs pss
                    FullPostGift _ _ -> False
        it "preserves history for postgift game state" $ property $
            \(ArbitraryObservedPostGiftState p0cs p1cs history) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateRandomFullGameState (PostGift p0cs p1cs history))) src
                assert $ case fullGameState of
                    FullPreGift _ _ -> False
                    FullPostGift _ hist -> hist == history
        it "preserves Player0 cards for postgift game state" $ property $
            \(ArbitraryObservedPostGiftState p0cs p1cs history) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateRandomFullGameState (PostGift p0cs p1cs history))) src
                assert $ case fullGameState of
                    FullPreGift _ _ -> False
                    FullPostGift pss _ -> playerCardsPreserved Player0 p0cs pss
        it "generates complete cards for postgift state" $ property $
            \(ArbitraryObservedPostGiftState p0cs p1cs history) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateRandomFullGameState (PostGift p0cs p1cs history))) src
                assert $ case fullGameState of
                    FullPreGift _ _ -> False
                    FullPostGift pss hist -> playerStatesAndHistoryComplete pss hist
        it "does not generate cards for missing suit in postgift state" $ property $
            \(ArbitraryObservedPostGiftState p0cs p1cs history) -> monadicIO $ do
                src <- newStdGen
                let fullGameState = evalState (sampleStateRVar (generateRandomFullGameState (PostGift p0cs p1cs history))) src
                assert $ case fullGameState of
                    FullPreGift _ _ -> False
                    FullPostGift pss _ -> playerStatesLegal pss history

correctlyObserved :: Player -> FullGameState -> FullGameState -> ObservedGameState -> Bool
correctlyObserved _ (FullPostGift {}) _ _ = False
correctlyObserved _ _ (FullPreGift {}) (PostGift {}) = False
correctlyObserved _ _ (FullPostGift {}) (PreGift {}) = False
correctlyObserved observer _ (FullPreGift pStart pss) (PreGift p0cs pStart') =
        pStart' == observePlayer observer pStart &&
        playerCardsPreserved observer p0cs pss
correctlyObserved observer (FullPreGift _ pssInit) (FullPostGift pss history) (PostGift p0cs p1cs observedHistory) =
        playerCardsPreserved observer p0cs pss &&
        isSubset p1cs (hand $ head $ filter ((==) observer . player) pssInit)

applyMoveWorks :: Move -> FullGameState -> FullGameState -> Bool
applyMoveWorks _ _ (FullPreGift _ _) = False
applyMoveWorks (Gift _) (FullPostGift _ _) _ = False
applyMoveWorks (Gift gift) (FullPreGift _ _) (FullPostGift pssEnd _) =
        all ((==) 13 . length . hand) pssEnd &&
        all (uncurry playerLostGifts) (zip sortedEndHands sortedGifts) &&
        all (uncurry playerGainedGifts) (zip sortedEndHands (rotate $ rotate $ rotate sortedGifts))
    where sortedEndHands = map hand $ sortOn player pssEnd
          sortedGifts = map snd $ sortOn fst gift

          playerLostGifts end giftGiven = not (any (`elem` giftGiven) end)
          playerGainedGifts end = all (`elem` end)

applyMoveWorks (Deal _ _) (FullPreGift _ _) _ = False
applyMoveWorks (Deal p c) (FullPostGift pssStart historyStart) (FullPostGift pssEnd historyEnd) =
        playerTurn == p &&
        c `elem` playerHand pssStart &&
        c `notElem` playerHand pssEnd &&
        c == lastCardInHistory
    where playerTurn = turn (FullPostGift pssStart historyStart)
          playerHand pss = hand $ head $ filter ((==) p . player) pss
          lastCardInHistory = if null $ cards $ head historyEnd then last $ cards $ historyEnd !! 1 else last $ cards $ head historyEnd

data ArbitraryStateMove = ArbitraryStateMove FullGameState Move
    deriving Show

instance Arbitrary ArbitraryStateMove where
    arbitrary :: Gen ArbitraryStateMove
    arbitrary = do
        (ArbitraryFullGameState fgs) <- arbitrary
        case moveOptions fgs of
            DealOptions p cs -> do
                chosenCard <- oneof $ map return cs
                return $ ArbitraryStateMove fgs (Deal p chosenCard)
            GiftOptions options -> do
                chosenGifts <- mapM (\(p, gifts) -> do
                        gift <- oneof $ map return gifts
                        return (p, gift)
                    ) options
                return $ ArbitraryStateMove fgs (Gift chosenGifts)

movesLegal :: FullGameState -> MoveOptions -> Bool
movesLegal (FullPreGift _ _) (DealOptions _ _) = False
movesLegal (FullPostGift _ _) (GiftOptions _) = False
movesLegal (FullPostGift pss history) (DealOptions p cs) =
        turnPlayer == p &&
        all (`elem` turnHand) cs
    where turnHand = hand $ head $ filter ((==) turnPlayer . player) pss
          turnPlayer = turn (FullPostGift pss history)
movesLegal (FullPreGift _ pss) (GiftOptions options) =
        isPermutation giftGivers players &&
        all (\(p', gifts) -> all (\gift -> isSubset gift (playerHand p')) gifts) options
    where giftGivers = map fst options
          playerHand p' =  hand $ head $ filter ((==) p' . player) pss


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