{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module LikhaGame
(
    Player(..),
    next,
    Table(..),
    nextPlayer,
    collect,
    tableScore,
    moves,
    gifts
) where

import Cards (Card(..), Suit (..), Number (..))
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import ListUtils ( pick )

data Player = Player0 | Player1 | Player2 | Player3
    deriving (Eq, Enum, Ord, Show)

next :: Player -> Player
next Player0 = Player1
next Player1 = Player2
next Player2 = Player3
next Player3 = Player0

data Table = Table {startingPlayer:: Player, cards :: [Card]}
    deriving Show

nextPlayer :: Table -> Player
nextPlayer table = iterate next (startingPlayer table) !! length (cards table)

collect :: Table -> Player
collect (Table start cs) = fst
                            $ maximumBy (comparing (number . snd))
                            $ filter ((==) tableSuit . suit . snd)
                            $ zip (iterate next start) cs
    where tableSuit = suit $ head cs

tableScore :: Table -> Int
tableScore = sum . map \case
                    Card Hearts _ -> 1
                    Card Spades Queen -> 13
                    Card Diamonds Ten -> 10
                    _ -> 0
                 . cards

moves :: Suit -> [Card] -> [Card]
moves s cs
    | not $ null cardsInSuit = cardsInSuit
    | otherwise = cs
    where
        cardsInSuit = filter ((== s) . suit) cs

gifts :: [Card] -> [[Card]]
gifts = pick 3