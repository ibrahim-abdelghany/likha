module LikhaGameHeuristics (
    gameStateHeuristic,
    giftHeuristic
) where

import GHC.Float (int2Float)
import Data.List ( (\\) )

import Cards (Card(..), Suit (..), Number (..), numberToInt)
import LikhaGame ( Player(..), Table(..), collect, tableScore)
import LikhaGameState (PlayerState(..), FullGameState (..))

-- basic algorithm to compute heuristic score of a hand of cards
gameStateHeuristic :: FullGameState -> [(Player, Float)]
gameStateHeuristic (FullPreGift _ playerStates) = [(player ps, int2Float $ handHeuristic [] $ hand ps) | ps <- playerStates]
gameStateHeuristic (FullPostGift playerStates history) =  map (\(p, weight, currentScore) -> (p, currentScore + remainingScore * weight / totalWeight)) playerCardWeights
    where playerCardWeights = map (\ps -> (player ps, int2Float $ handHeuristic history $ hand ps, int2Float $ score ps + if winner == player ps then predictedScore else 0)) playerStates

          totalWeight = sum $ map (\(_,b,_)-> b) playerCardWeights

          remainingScore = int2Float $ 36 - sum (map score playerStates)

          predictedScore = if not (null history) && length (cards $ head history) < 4 then tableScore $ head history else 0

          winner = if not (null history) && not (null $ cards $ head history) && length (cards $ head history) < 4 then collect (head history) else Player0

giftHeuristic :: [Card] -> [Card] -> Int
giftHeuristic cs gift = handHeuristic [] $ cs \\ gift

handHeuristic :: [Table] -> [Card]  -> Int
handHeuristic history cs = basicCost + likhaSuitCost (Card Spades Queen) + likhaSuitCost (Card Diamonds Ten)
  where basicCost = sum $ map (\(Card s n) -> suitCost s * numberToInt n) cs
        likhaSuitCost likha = if not $ likhaPlayed likha then likhaCost likha * (\(under, above) -> max 0 above - under) (cardsUnderAboveLikha likha) else 0

        cardsUnderAboveLikha :: Card -> (Int, Int)
        cardsUnderAboveLikha likha = (under, above)
          where inSuit = filter ((==) (suit likha) . suit) cs
                under = length $ filter ((<) (number likha) . number) inSuit
                above = length $ filter ((>=) (number likha) . number) inSuit

        suitCost :: Num a => Suit -> a
        suitCost Hearts = 5
        suitCost _      = 1

        likhaCost :: Num a => Card -> a
        likhaCost (Card Spades Queen) = 13 * 5
        likhaCost (Card Diamonds Ten) = 10 * 5
        likhaCost _                   = 0

        likhaPlayed likha = any (\(Table _ tcs) -> likha `elem` tcs) history