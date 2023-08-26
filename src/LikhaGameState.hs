module LikhaGameState (
    PlayerState(..),
    ObservedGameState(..),
    FullGameState(..),
    generateFullGameState,
    turn,
    hands,
    missingSuits
) where

import Data.Random ( RVar, randomElement )
import Data.Random.List (shuffle)

import Data.Maybe (mapMaybe, fromJust, isJust)

import Cards (Card(..), deck, suit, Suit)
import LikhaGame ( Player(..), next, Table(..), History, collect, tableScore, nextPlayer)
import Data.Foldable (find)
import Data.List ((\\), minimumBy)
import Data.Ord (comparing)
import GHC.Float (int2Float)
import Control.Monad.Extra (iterateMaybeM)

data PlayerState = PlayerState {player :: Player, hand :: [Card], score :: Int}
    deriving Show

data ObservedGameState = PreGift [Card] Player | PostGift [Card] [Card] History
    deriving Show

data FullGameState = FullPreGift Player [PlayerState] | FullPostGift [PlayerState] History
    deriving Show

turn :: FullGameState -> Player
turn (FullPreGift p _) = p
turn (FullPostGift _ history) = nextPlayer $ head history

hands :: FullGameState -> [PlayerState]
hands (FullPreGift _ pss) = pss
hands (FullPostGift pss _) = pss

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

generateFullGameState (PostGift p0cs p1cs history) = fmap fromJust $ repeatUntil isJust $ do
    maybeFullplayerHands <- distributeFreeCards [
            (Player1, nPlayer1Cards, playerDomain Player1 freeCards),
            (Player2, nPlayer2Cards, playerDomain Player2 freeCards),
            (Player3, nPlayer3Cards, playerDomain Player3 freeCards)
        ]
    return $ do
        fullplayerHands <- maybeFullplayerHands
        let playerHand p = snd $ head $ filter ((==) p . fst) fullplayerHands
        return $ FullPostGift [
            PlayerState Player0 p0cs (playerScore Player0),
            PlayerState Player1 (playerHand Player1 ++ p1csLeft) (playerScore Player1),
            PlayerState Player2 (playerHand Player2) (playerScore Player2),
            PlayerState Player3 (playerHand Player3) (playerScore Player3)
          ] history

  where playerScore p = sum $ map (\table -> if collect table == p then tableScore table else 0) history

        playerDomain :: Player -> [Card] -> [Card]
        playerDomain p = filter (\c -> suit c `notElem` missingSuits p history)

        nPlayer1Cards = remainingCardsPerPlayer + extraCard Player1 - length p1csLeft
        nPlayer2Cards = remainingCardsPerPlayer + extraCard Player2
        nPlayer3Cards = remainingCardsPerPlayer + extraCard Player3

        extraCard p = if not (null players) && p `notElem` players then 1 else 0
          where currentTable = head history
                players = take (length $ cards currentTable) (iterate next $ startingPlayer currentTable)

        remainingCardsPerPlayer = remainingCards `div` 4
        remainingCards = 52 - length playedCards

        p1csLeft = p1cs \\ playedCards

        freeCards = deck \\ usedCards
        usedCards = p0cs ++ p1cs ++ playedCards
        playedCards = concatMap cards history

repeatUntil :: (Monad m) => (b -> Bool) -> m b -> m b
repeatUntil test generator = do
    x <- generator
    if test x then return x else repeatUntil test generator

distributeFreeCards :: [(Player, Int, [Card])] -> RVar (Maybe [(Player, [Card])])
distributeFreeCards domains = do
    -- if final game state return empty hands
    if all (\(_, hnum, _) -> hnum == 0) domains
    then return $ Just [(p,[]) | (p,_,_) <- domains]
    else do
        let initialDomains = map (\(p, hnum, dom) -> (p, hnum, [], dom)) domains
        finalDomains <- iterateMaybeM pickCardForMostConstrainedPlayer initialDomains
        let fullDomains = all (\(_, n, h, _) -> length h == n)
        let pickCards = map (\(p, _, h, _) -> (p, h))
        if fullDomains (last finalDomains) then return $ Just $ pickCards (last finalDomains) else return Nothing

pickCardForMostConstrainedPlayer :: [(Player, Int, [Card], [Card])] -> RVar (Maybe [(Player, Int, [Card], [Card])])
pickCardForMostConstrainedPlayer domains = do
    let validPlayers = filter (\(_, _hnum, _hand, _dom) -> not (null _dom) && _hnum /= length _hand) domains
    if null validPlayers then return Nothing else do
        let (p, hnum, phand, dom) = minimumBy (comparing (\(_, _hnum, _hand, _dom) -> int2Float (length _dom) / int2Float (_hnum - length _hand) )) validPlayers
        card <- randomElement dom
        let otherPlayerDomains = filter (\(p', _, _, _) -> p' /= p) domains
        let otherPlayersUpdatedDomains = map (\(p', hnum', phnd', dom') -> (p', hnum', phnd', dom' \\ [card])) otherPlayerDomains
        return $ Just $ (p, hnum, card:phand, dom \\ [card]) : otherPlayersUpdatedDomains

missingSuits :: Player -> History -> [Suit]
missingSuits p = mapMaybe (missingFromTable p)

missingFromTable :: Player -> Table -> Maybe Suit
missingFromTable p (Table start cs) = do
    let playerSuits = zip (iterate next start) (map suit cs)
    tableSuit <- snd <$> if not (null playerSuits) then Just (head playerSuits) else Nothing
    playerSuit <- snd <$> find ((==) p . fst) playerSuits
    if playerSuit /= tableSuit then Just tableSuit else Nothing

