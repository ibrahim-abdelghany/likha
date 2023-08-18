{-# LANGUAGE LambdaCase #-}
module LikhaGameState (
    PlayerState(..),
    ObservedGameState(..),
    FullGameState(..),
    generateFullGameState,
    turn,
    missingSuits
) where

import Data.Random ( RVar, randomElement )
import Data.Random.List (shuffle)

import Data.Maybe (mapMaybe)

import Cards (Card(..), deck, suit, Suit)
import LikhaGame ( Player(..), next, Table(..), collect, tableScore, nextPlayer)
import Data.Foldable (find)
import Data.List ((\\), minimumBy)
import Data.Ord (comparing)

data PlayerState = PlayerState {player :: Player, hand :: [Card], score :: Int}
    deriving Show

data ObservedGameState = PreGift [Card] Player | PostGift [Card] [Card] [Table]
    deriving Show

data FullGameState = FullPreGift Player [PlayerState] | FullPostGift [PlayerState] [Table]
    deriving Show

turn :: FullGameState -> Player
turn (FullPreGift p _) = p
turn (FullPostGift _ history) = nextPlayer $ head history

generateFullGameState :: ObservedGameState -> RVar FullGameState
generateFullGameState (PreGift p0cs p) = do
    shuffledCards <- shuffle $ deck \\ p0cs
    let player1Cards = take 13 shuffledCards
    let player2Cards = take 13 $ drop 13 shuffledCards
    let player3Cards = drop 26 shuffledCards

    return $ FullPreGift p [
        PlayerState Player0 p0cs 0,
        PlayerState Player1 player1Cards 0,
        PlayerState Player2 player2Cards 0,
        PlayerState Player3 player3Cards 0
      ]

generateFullGameState (PostGift p0cs p1cs history) = repeatUntil (
        \case 
            FullPreGift _ _ -> False
            FullPostGift pss _ -> playerStatesLegal pss history)
        $ do
  let player1Domain = playerDomain Player1 freeCards
  shuffledRemainingCardsP1 <- shuffle player1Domain
  let player1Cards = take nPlayer1Cards shuffledRemainingCardsP1 ++ p1csLeft

  let player2Domain = playerDomain Player2 $ freeCards \\ player1Cards
  shuffledRemainingCardsP2 <- shuffle player2Domain
  let player2Cards = take nPlayer2Cards shuffledRemainingCardsP2

  let player3Cards = freeCards \\ (player1Cards ++ player2Cards)

  return $ FullPostGift [
        PlayerState Player0 p0cs (playerScore Player0),
        PlayerState Player1 player1Cards (playerScore Player1),
        PlayerState Player2 player2Cards (playerScore Player2),
        PlayerState Player3 player3Cards (playerScore Player3)
      ] history

  where playerScore p = sum $ map (\table -> if collect table == p then tableScore table else 0) history

        playerDomain :: Player -> [Card] -> [Card]
        playerDomain p = filter (\c -> suit c `notElem` missingSuits p history)

        nPlayer1Cards = remainingCardsPerPlayer + extraCard Player1 - length p1csLeft
        nPlayer2Cards = remainingCardsPerPlayer + extraCard Player2

        extraCard p = if not (null players) && p `notElem` players then 1 else 0
          where currentTable = head history
                players = take (length $ cards currentTable) (iterate next $ startingPlayer currentTable)

        remainingCardsPerPlayer = remainingCards `div` 4
        remainingCards = 52 - length playedCards

        p1csLeft = p1cs \\ playedCards

        freeCards = deck \\ usedCards
        usedCards = p0cs ++ p1cs ++ playedCards
        playedCards = concatMap cards history

repeatUntil :: (Monad m, Show b) => (b -> Bool) -> m b -> m b
repeatUntil test generator = do
    x <- generator
    if test x then return x else repeatUntil test generator

playerStatesLegal :: [PlayerState] -> [Table] -> Bool
playerStatesLegal pss history = all (`playerStateDoesntHaveIllegalSuits` history) pss

playerStateDoesntHaveIllegalSuits :: PlayerState -> [Table] -> Bool
playerStateDoesntHaveIllegalSuits (PlayerState p cs _) history = all ((`notElem` missing) . suit) cs
    where missing = missingSuits p history

missingSuits :: Player -> [Table] -> [Suit]
missingSuits p = mapMaybe (missingFromTable p)

missingFromTable :: Player -> Table -> Maybe Suit
missingFromTable p (Table start cs) = do
    let playerSuits = zip (iterate next start) (map suit cs)
    tableSuit <- snd <$> if not (null playerSuits) then Just (head playerSuits) else Nothing
    playerSuit <- snd <$> find ((==) p . fst) playerSuits
    if playerSuit /= tableSuit then Just tableSuit else Nothing

