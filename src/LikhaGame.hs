{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module LikhaGame
(
    Player(..),
    players,
    next,
    Table(..),
    History,
    nextPlayer,
    collect,
    pushToTable,
    tableScore,
    moves,
    gifts,
    likhas
) where

import Cards (Card(..), Suit (..), Number (..), suit, number)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import ListUtils ( pick )

data Player = Player0 | Player1 | Player2 | Player3
    deriving (Eq, Enum, Ord, Show)

players :: [Player]
players = [Player0, Player1, Player2, Player3]

next :: Player -> Player
next Player0 = Player1
next Player1 = Player2
next Player2 = Player3
next Player3 = Player0

type History = [Table]

data Table = Table {startingPlayer:: Player, cards :: [Card]}
    deriving (Show, Eq)

nextPlayer :: Table -> Player
nextPlayer table = iterate next (startingPlayer table) !! length (cards table)

collect :: Table -> Player
collect (Table start []) = start
collect (Table start cs) = fst
                            $ maximumBy (comparing (number . snd))
                            $ filter ((==) tableSuit . suit . snd)
                            $ zip (iterate next start) cs
    where tableSuit = suit $ head cs

pushToTable :: Card -> Table -> Table
pushToTable card (Table start cs) = Table start (cs ++ [card])

tableScore :: Table -> Int
tableScore = sum . map \case
                    Card Hearts _ -> 1
                    Card Spades Queen -> 13
                    Card Diamonds Ten -> 10
                    _ -> 0
                 . cards

moves :: History -> [Card] -> [Card]
moves history cs
  | null $ cards $ head history = cs
  | not $ null likhasCollectedByOthers = likhasCollectedByOthers
  | otherwise = legalForSuit
  where
      currentPlayer = nextPlayer $ head history
      legalForSuit = legalMovesForSuit (suit $ head $ cards $ head history) cs
      likhasForSuit = filter (`elem` likhas) legalForSuit
      likhasCollectedByOthers = filter (\likha -> collect (pushToTable likha (head history)) /= currentPlayer) likhasForSuit

legalMovesForSuit :: Suit -> [Card] -> [Card]
legalMovesForSuit s cs
    | not $ null cardsInSuit = cardsInSuit
    | otherwise = cs
    where
        cardsInSuit = filter ((== s) . suit) cs

gifts :: [Card] -> [[Card]]
gifts = pick 3

likhas :: [Card]
likhas = [Card Spades Queen, Card Diamonds Ten]