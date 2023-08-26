module LikhaGameHeuristics (
    gameStateHeuristic,
    giftHeuristic,
    handHeuristic
) where

import GHC.Float (int2Float)
import Data.List ( (\\) )

import Cards (Card(..), Suit (..), Number (..), numberToInt, suit, number)
import LikhaGame ( Player(..), Table(..), History, collect, tableScore)
import LikhaGameState (PlayerState(..), FullGameState (..))

-- basic algorithm to compute heuristic score of a hand of cards
-- assume history is never empty
gameStateHeuristic :: FullGameState -> [(Player, Float)]
gameStateHeuristic (FullPreGift _ playerStates) = map (\(p,w) -> (p, 36 * w / totalWeight)) playerWeights
    where playerWeights = [(player ps, int2Float $ handHeuristic [] $ hand ps) | ps <- playerStates]
          totalWeight = sum $ map snd playerWeights
gameStateHeuristic (FullPostGift playerStates history)
  | totalWeight == 0    = map (\(PlayerState p _ s) -> (p, int2Float s)) playerStates
  | otherwise           = map (\(p, weight, currentScore) -> (p,  currentScore +  remainingScore * weight / totalWeight)) playerCardWeightsScores
    where playerCardWeightsScores = map (triapply player (int2Float . handWeight) (int2Float . expectedScore)) playerStates

          handWeight :: PlayerState -> Int
          handWeight = handHeuristic history . hand

          expectedScore :: PlayerState -> Int
          expectedScore ps = score ps + if winner == player ps then predictedScore else 0

          totalWeight = sum $ map (\(_,b,_)-> b) playerCardWeightsScores

          remainingScore = int2Float $ 36 - sum (map score playerStates) - predictedScore

          predictedScore = tableScore $ head history
          winner = collect $ head history

giftHeuristic :: [Card] -> [Card] -> Int
giftHeuristic cs gift = handHeuristic [] $ cs \\ gift

handHeuristic :: History -> [Card]  -> Int
handHeuristic history cs = basicCost + likhaSuitCost (Card Spades Queen) + likhaSuitCost (Card Diamonds Ten)
  where basicCost = sum $ map (\(Card s n) -> suitCost s * numberToInt n) cs

        likhaSuitCost likha = if not $ likhaPlayed likha then likhaCost likha * cardsRelativeCost likha else 0

        cardsRelativeCost :: Card -> Int
        cardsRelativeCost likha = max 0 (above - under) -- if above < under then 1 else 0
          where inSuit = filter ((==) (suit likha) . suit) cs
                under = length $ filter ((>) (number likha) . number) inSuit -- counter intuitive use of >, but it will take the first argument to the left
                above = length $ filter ((<=) (number likha) . number) inSuit

        suitCost :: Num a => Suit -> a
        suitCost Hearts = 13
        suitCost _      = 1

        likhaCost :: Num a => Card -> a
        likhaCost (Card Spades Queen) = 13 * 13
        likhaCost (Card Diamonds Ten) = 10 * 13
        likhaCost _                   = 0

        likhaPlayed likha = any (\(Table _ tcs) -> likha `elem` tcs) history

triapply :: (a -> d) -> (a -> e) -> (a -> f) -> a -> (d, e, f)
triapply f1 f2 f3 x = (f1 x, f2 x, f3 x)