{-# LANGUAGE InstanceSigs #-}
module ArbitraryGameState (
    ArbitraryObservedPreGiftState(..),
    ArbitraryObservedPostGiftState(..),
    ShuffledDeck(..),
    ArbitraryFullPostGiftState(..),
) where

import Test.QuickCheck (Arbitrary (arbitrary), Gen, shuffle, oneof)
import Test.QuickCheck.Gen (choose)

import LikhaGame( Player(..), players, next, Table(..), moves, tableScore, collect )
import Cards (Card, deck, suit)
import LikhaGameState(PlayerState(..))

import ListUtils (rotate)
import Data.List (sortOn, (\\))

import Control.Monad.Extra (iterateM)
import Data.Maybe (fromJust, mapMaybe)
import Data.Foldable (find)

newtype ShuffledDeck = ShuffledDeck ([Card], [Card], [Card], [Card])
    deriving (Show)

instance Arbitrary ShuffledDeck where
  arbitrary :: Gen ShuffledDeck
  arbitrary = do
    shuffledDeck <- shuffle deck
    let p0cs = take 13 shuffledDeck
    let p1cs = take 13 $ drop 13 shuffledDeck
    let p2cs = take 13 $ drop 26 shuffledDeck
    let p3cs = drop 39 shuffledDeck
    return $ ShuffledDeck (p0cs, p1cs, p2cs, p3cs)

data ArbitraryObservedPreGiftState = ArbitraryObservedPreGiftState Player [Card]
    deriving Show

instance Arbitrary ArbitraryObservedPreGiftState where
  arbitrary :: Gen ArbitraryObservedPreGiftState
  arbitrary = do
    shuffledDeck <- shuffle deck
    starting <- oneof $ map return players
    let p0cs = take 13 shuffledDeck
    return $ ArbitraryObservedPreGiftState starting p0cs

data ArbitraryObservedPostGiftState = ArbitraryObservedPostGiftState [Card] [Card] [Table]
    deriving Show

instance Arbitrary ArbitraryObservedPostGiftState where
  arbitrary :: Gen ArbitraryObservedPostGiftState
  arbitrary = do
    ArbitraryFullPostGiftState pss history <- arbitrary
    let p0cs = hand $ fromJust $ find ((==) Player0 . player) pss
    let p1cs = hand $ fromJust $ find ((==) Player1 . player) pss
    let p1hist = cardsFromHistory Player1 history
    let allp1cs = p1cs ++ p1hist
    p1Gift <- shuffle allp1cs
    return $ ArbitraryObservedPostGiftState p0cs (take 3 p1Gift \\ p1hist) history

cardsFromHistory :: Player -> [Table] -> [Card]
cardsFromHistory p = mapMaybe (\(Table start cs) -> snd <$> find ((==) p . fst) (zip (iterate next start) cs))

data ArbitraryFullPostGiftState = ArbitraryFullPostGiftState [PlayerState] [Table]
    deriving (Show)

instance Arbitrary ArbitraryFullPostGiftState where
  arbitrary :: Gen ArbitraryFullPostGiftState
  arbitrary = do
    ShuffledDeck (p0cs, p1cs, p2cs, p3cs) <- arbitrary

    starting <- oneof $ map return players

    let startingStates = [PlayerState Player0 p0cs 0, PlayerState Player1 p1cs 0, PlayerState Player2 p2cs 0, PlayerState Player3 p3cs 0]

    let emptyHistory = [Table starting []]

    rounds <- choose (0, 4 * 13 - 1)

    randomTables <- iterateM randomTable (startingStates, emptyHistory)

    let (states, history) = randomTables !! rounds

    return $ ArbitraryFullPostGiftState states history

randomTable :: ([PlayerState], [Table]) -> Gen ([PlayerState], [Table])
randomTable (_, []) = error "history should be nonempty"
randomTable (playerStates, (Table starting cs):history) = do
    let sortedPlayers = iterate rotate (sortOn player playerStates) !! fromEnum starting

    let turn = sortedPlayers !! length cs

    let choices = if null cs then hand turn else moves (suit (head cs)) $ hand turn

    turnCard <- oneof $ map return choices

    let newtable = Table starting (cs ++ [turnCard])

    let winner = collect newtable
    let scoreT = tableScore newtable
    let final = length (cards newtable) == 4

    let newHistory = if final then Table winner []:newtable:history else newtable:history

    let newStates = [PlayerState
            (player ps)
            (if player ps == player turn then hand ps \\ [turnCard] else hand ps)
            (score ps + if final && player ps == winner then scoreT else 0) | ps <- playerStates]

    return (newStates, newHistory)
