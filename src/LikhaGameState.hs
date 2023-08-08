module LikhaGameState (
    PlayerState(..),
    ObservedGameState(..),
    FullGameState(..),
    sampleGameState,
    currentPlayer
) where

import Data.List ( (\\) )

import Data.Random ( RVar )
import Data.Random.List (shuffle)

import Data.Maybe (mapMaybe)

import Cards (Card(..), deck)
import LikhaGame ( Player(..), next, Table(..), collect, tableScore, nextPlayer)

data PlayerState = PlayerState {player :: Player, hand :: [Card], score ::Int}
    deriving Show

data ObservedGameState = PreGift [Card] Player | PostGift [Card] [Card] [Table]
    deriving Show

data FullGameState = FullPreGift Player [PlayerState] | FullPostGift [PlayerState] [Table]
    deriving Show

-- TODO rename
currentPlayer :: FullGameState -> Player
currentPlayer (FullPreGift p _) = p
currentPlayer (FullPostGift _ history) = nextPlayer $ head history

sampleGameState :: ObservedGameState -> RVar FullGameState
sampleGameState (PreGift p0cs p) = do
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

sampleGameState (PostGift p0cs p1cs history) = do
  shuffledRemainingCardsP1 <- shuffle $ playerDomain Player1 freeCards
  let player1Cards = take nPlayer1Cards shuffledRemainingCardsP1 ++ p1csLeft
  
  shuffledRemainingCardsP2 <- shuffle $ playerDomain Player2 $ freeCards \\ player1Cards
  let player2Cards = take nPlayer2Cards shuffledRemainingCardsP2
  let player3Cards = drop nPlayer2Cards shuffledRemainingCardsP2 
  
  return $ FullPostGift [
        PlayerState Player0 p0cs (playerScore Player0),
        PlayerState Player1 player1Cards (playerScore Player1),
        PlayerState Player2 player2Cards (playerScore Player2),
        PlayerState Player3 player3Cards (playerScore Player3)
      ] history
  
  where playerScore p = sum $ map (\table -> if collect table == p then tableScore table else 0) history

        playerDomain p = filter (\c -> suit c `elem` missingSuits)
          where missingSuits = mapMaybe (missingSuit . playerSuits) history
                missingSuit pss = if tableSuit /= playerSuit then Just tableSuit else Nothing
                  where tableSuit = snd $ head pss
                        playerSuit = snd $ head $ filter ((==) p . fst) pss
                playerSuits (Table p' cs) = zip (iterate next p') $ map suit cs
        
        nPlayer1Cards = remainingCardsPerPlayer - length p1csLeft + extraCard Player1
        nPlayer2Cards = remainingCardsPerPlayer + extraCard Player2

        extraCard p = if length players == 4 || notElem p players then 0 else 1
          where lastTable = last history
                players = take (length $ cards lastTable) (iterate next $ startingPlayer lastTable) 

        remainingCardsPerPlayer = remainingCards `div` 4
        remainingCards = 52 - length playedCards

        p1csLeft = playedCards \\ playedCards
        
        freeCards = deck \\ usedCards
        usedCards = p0cs ++ p1cs ++ playedCards
        playedCards = concatMap cards history
