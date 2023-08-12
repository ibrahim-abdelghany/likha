{-# LANGUAGE InstanceSigs #-}
module LikhaGameHeuristicsSpec (spec) where

import Test.Hspec ( describe, it, Spec, shouldSatisfy )
import Test.QuickCheck (Testable(property), Arbitrary (arbitrary), Gen, shuffle )

import Cards
    ( Card(Card),
      Number(King, Four, Ace, Two, Queen, Ten, Jack),
      Suit(Clubs, Hearts, Diamonds, Spades),
      deck )
import LikhaGame ( Table(Table), Player(Player1, Player0) )
import LikhaGameHeuristics (handHeuristic, gameStateHeuristic)
import LikhaGameState (FullGameState(..), PlayerState (..))
import GHC.Float (int2Float)

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
        it "returns proportional scores to hand scores for PreGift state" $ property $
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
        it "returns scores that add up to 36 for PreGift state" $ property $
            \(ShuffledDeck (p0cs, p1cs, p2cs, p3cs)) ->
                gameStateHeuristic (FullPreGift Player0 [
                    PlayerState Player0 p0cs 0,
                    PlayerState Player1 p1cs 0,
                    PlayerState Player1 p2cs 0,
                    PlayerState Player1 p3cs 0
                ]) `shouldSatisfy` \h -> 36 ~== sum (map snd h)

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
