module Cards
(
    Suit(..),
    suits,
    Number(..),
    numbers,
    numberToInt,
    Card(..),
    deck
) where

data Suit = Hearts | Clubs | Spades | Diamonds
    deriving (Show, Eq, Enum)

suits :: [Suit]
suits = [Hearts .. Diamonds]

data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord, Enum)

numbers :: [Number]
numbers = [Two .. Ace]

numberToInt :: Number -> Int
numberToInt n = 1 + fromEnum n

data Card = Card {suit :: Suit, number :: Number}
    deriving (Show, Eq)

deck :: [Card]
deck = [Card s n | s <- suits, n <- numbers]