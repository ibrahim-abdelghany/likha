{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LikhaGameHeuristicsSpec (spec) where

import Test.Hspec ( describe, it, Spec, shouldSatisfy )
import Test.QuickCheck (Testable(property), Arbitrary (arbitrary), Gen, shuffle )

import Cards(Card(..), Number(..), Suit(..), deck, suit)
import LikhaGame ( Table(..), Player(..), moves, collect, tableScore )
import LikhaGameHeuristics (handHeuristic, gameStateHeuristic)
import LikhaGameState (FullGameState(..), PlayerState (..))

import GHC.Float (int2Float)
import Test.QuickCheck.Gen (choose, oneof)
import ListUtils (rotate)
import Data.List (sortOn, (\\))

import Control.Monad.Extra(iterateM)
import Debug.Trace (traceShowM)

spec :: Spec
spec = do
    describe "handHeuristic" $ do
        it "prefers lower cards" $
            handHeuristic [] [Card Clubs Ace, Card Clubs Ten] `shouldSatisfy`
                (> handHeuristic [] [Card Clubs Two, Card Clubs Four])
        it "hates hearts" $
            handHeuristic [] [Card Hearts Two, Card Hearts Four] `shouldSatisfy`
                (> handHeuristic [] [Card Clubs Two, Card Clubs Four])
        it "hates low hearts more than high Clubs" $
            handHeuristic [] [Card Hearts Two, Card Hearts Four] `shouldSatisfy`
                (> handHeuristic [] [Card Clubs Ace, Card Clubs Jack])
        it "hates Queen of Spades" $
            handHeuristic [] [Card Spades Queen] `shouldSatisfy`
                (> handHeuristic [] [Card Spades Two])
        it "hates cards higher than Queen of Spades" $
            handHeuristic [] [Card Spades King] `shouldSatisfy`
                (> handHeuristic [] [Card Clubs King])
        it "uses low cards to tolerate cards higher than Queen of Spades" $
            handHeuristic [] [Card Spades King, Card Spades Jack] `shouldSatisfy`
                (== handHeuristic [] [Card Clubs King, Card Clubs Jack])
        it "hates Ten of Diamonds" $
            handHeuristic [] [Card Diamonds Ten] `shouldSatisfy`
                (> handHeuristic [] [Card Diamonds Two])
        it "hates Ten of Diamonds less than Queen of Spades" $
            handHeuristic [] [Card Diamonds Ten] `shouldSatisfy`
                (< handHeuristic [] [Card Spades Queen])
        it "ignores higher than likha when likha played" $
            handHeuristic [Table Player0 [Card Spades Queen, Card Diamonds Ten]] [Card Diamonds Jack, Card Spades King] `shouldSatisfy`
                (== handHeuristic [] [Card Clubs Jack, Card Clubs King])
    describe "gameStateHeuristic" $ do
        it "returns proportional heuristics to hand scores for PreGift state" $ property $
            \(ShuffledDeck (p0cs, p1cs, p2cs, p3cs)) ->
                gameStateHeuristic (FullPreGift Player0 [
                    PlayerState Player0 p0cs 0,
                    PlayerState Player1 p1cs 0,
                    PlayerState Player1 p2cs 0,
                    PlayerState Player1 p3cs 0
                ]) `shouldSatisfy` \h -> map snd h `proportional` map int2Float [
                    handHeuristic [] p0cs,
                    handHeuristic [] p1cs,
                    handHeuristic [] p2cs,
                    handHeuristic [] p3cs
                ]
        it "returns heuristics that add up to 36 for PreGift state" $ property $
            \(ShuffledDeck (p0cs, p1cs, p2cs, p3cs)) ->
                gameStateHeuristic (FullPreGift Player0 [
                    PlayerState Player0 p0cs 0,
                    PlayerState Player1 p1cs 0,
                    PlayerState Player1 p2cs 0,
                    PlayerState Player1 p3cs 0
                ]) `shouldSatisfy` \h -> 36 ~== sum (map snd h)
        it "returns heuristics that add up to 36 for a PostGiftState" $ property $
            \(ArbitraryPostGiftState states history) ->
                gameStateHeuristic (FullPostGift states history) `shouldSatisfy`
                     \heuristic -> 36 ~== sum (map snd heuristic)
        it "returns heuristics that are greater or equal to scores for a PostGiftState" $ property
            arbitraryPostGiftStateHasHigherScores 
                where arbitraryPostGiftStateHasHigherScores (ArbitraryPostGiftState states history) = 
                        gameStateHeuristic (FullPostGift states history) `shouldSatisfy` heuristicHigher 
                        where heuristicHigher heuristic = map snd heuristicSorted >= map (int2Float . score) scoresSorted
                                where heuristicSorted = sortOn fst heuristic
                                      scoresSorted = sortOn player states

(~==) :: Ord a => Fractional a => a -> a -> Bool
(~==) x y = abs (x-y) <= 1e-2

normalize :: Fractional a => [a] -> [a]
normalize xs = map (/ sum xs) xs

proportional ::  Ord a => Fractional a => [a] -> [a] -> Bool
proportional xs ys = all (uncurry (~==)) $ zip (normalize xs) (normalize ys)

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

data ArbitraryPostGiftState = ArbitraryPostGiftState [PlayerState] [Table]
    deriving (Show)

instance Arbitrary ArbitraryPostGiftState where
  arbitrary :: Gen ArbitraryPostGiftState
  arbitrary = do
    ShuffledDeck (p0cs, p1cs, p2cs, p3cs) <- arbitrary

    starting <- oneof $ map return [Player0, Player1, Player2, Player3]

    let startingStates = [PlayerState Player0 p0cs 0, PlayerState Player1 p1cs 0, PlayerState Player2 p2cs 0, PlayerState Player3 p3cs 0]

    let emptyHistory = [Table starting []]

    rounds <- choose (0, 12)

    randomTables <- iterateM randomTable (startingStates, emptyHistory)

    let (states, history) = randomTables !! rounds

    return $ ArbitraryPostGiftState states history

randomTable :: ([PlayerState], [Table]) -> Gen ([PlayerState], [Table])
randomTable (playerStates, (Table starting []):history) = do
    let sortedPlayers = iterate rotate (sortOn player playerStates) !! fromEnum starting
    traceShowM sortedPlayers

    let turn0 = head sortedPlayers
    turn0Card <- oneof $ map return $ hand turn0

    let turn1 = sortedPlayers !! 1
    turn1Card <- oneof $ map return $ moves (suit turn0Card) $ hand turn1

    let turn2 = sortedPlayers !! 2
    turn2Card <- oneof $ map return $ moves (suit turn0Card) $ hand turn2

    let turn3 = sortedPlayers !! 3
    turn3Card <- oneof $ map return $ moves (suit turn0Card) $ hand turn3

    let newtable = Table starting [turn0Card, turn1Card, turn2Card, turn3Card]

    let winner = collect newtable
    let scoreT p = if p == winner then tableScore newtable else 0

    return ([
            PlayerState (player turn0) (hand turn0 \\ [turn0Card]) (score turn0 + scoreT (player turn0)),
            PlayerState (player turn1) (hand turn1 \\ [turn1Card]) (score turn1 + scoreT (player turn1)),
            PlayerState (player turn2) (hand turn2 \\ [turn2Card]) (score turn2 + scoreT (player turn2)),
            PlayerState (player turn3) (hand turn3 \\ [turn3Card]) (score turn3 + scoreT (player turn3))
        ], Table winner []:newtable:history)