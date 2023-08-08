module LikhaGameHeuristicsSpec (spec) where

import Test.Hspec ( describe, it, Spec, shouldSatisfy )

import Cards
import LikhaGame
import LikhaGameHeuristics (handHeuristic)

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
