module LikhaGameSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )

import Cards (Card(..), Suit(..), Number(..))
import LikhaGame (Player(..), Table(..), nextPlayer, collect, tableScore, moves)

spec :: Spec
spec = do
    describe "nextPlayer" $ do
        it "returns first player if table empty" $
            nextPlayer (Table Player2 []) `shouldBe` Player2
        it "returns next player" $
            nextPlayer (Table Player0 [Card Hearts Two]) `shouldBe` Player1
    describe "collect" $ do
        it "chooses player with highest card in suit" $
            collect (Table Player1 [
                Card Spades Three, 
                Card Hearts Ace, 
                Card Spades Seven, 
                Card Clubs Jack
            ]) `shouldBe` Player3
    describe "tableScore" $ do
        it "returns 1 for each card of Hearts" $
            tableScore (Table Player1 [
                Card Spades Three, 
                Card Hearts Ace, 
                Card Spades Seven, 
                Card Clubs Jack
            ]) `shouldBe` 1
        it "returns 13 for each Queen of Spades" $
            tableScore (Table Player1 [
                Card Spades Queen, 
                Card Clubs Ace, 
                Card Spades Seven, 
                Card Clubs Jack
            ]) `shouldBe` 13
        it "returns 10 for each Ten of Diamonds" $
            tableScore (Table Player1 [
                Card Spades Ten, 
                Card Diamonds Ten, 
                Card Spades Seven, 
                Card Clubs Jack
            ]) `shouldBe` 10
        it "returns sum of scores for each Queen of Spades" $
            tableScore (Table Player1 [
                Card Spades Queen, 
                Card Diamonds Ten, 
                Card Hearts Seven, 
                Card Clubs Jack
            ]) `shouldBe` 24
    describe "moves" $ do
        it "returns cards from the same suit" $
            moves Spades [
                Card Spades Three, 
                Card Hearts Ace, 
                Card Spades Seven, 
                Card Clubs Jack
            ] `shouldBe` [
                Card Spades Three, 
                Card Spades Seven
            ]
        it "returns all cards if no cards from the same suit exist" $
            moves Diamonds [
                Card Spades Three, 
                Card Hearts Ace, 
                Card Spades Seven, 
                Card Clubs Jack
            ] `shouldBe` [
                Card Spades Three, 
                Card Hearts Ace, 
                Card Spades Seven, 
                Card Clubs Jack
            ]
        