{-# LANGUAGE TupleSections #-}
module LikhaGameState (
    PlayerState(..),
    ObservedGameState(..),
    usedCards,
    freeCards,
    FullGameState(..),
    Move(..),
    MoveOptions(..),
    playerState,
    isFinalState,
    playerScores,
    moveOptions,
    applyMove,
    observePlayer,
    observeFullGameState,
    generateRandomFullGameState,
    turn,
    hands,
    missingSuits,
    missingLikhas
) where

import Data.Random ( RVar, randomElement )
import Data.Random.List (shuffle)
import Data.Maybe (mapMaybe, fromJust, isJust)
import Data.Foldable (find)
import Data.List ((\\), minimumBy, sortOn)
import Data.Ord (comparing)

import GHC.Float (int2Float)
import Control.Monad.Extra (iterateMaybeM)

import Cards (Card(..), deck, suit, Suit (..))
import LikhaGame ( Player(..), next, Table(..), History, players, collect, tableScore, nextPlayer, gifts, moves, players, pushToTable, likhas)
import ListUtils (rotate)

data PlayerState = PlayerState {player :: Player, hand :: [Card], score :: Int}
    deriving Show

data ObservedGameState = PreGift [Card] Player | PostGift [Card] [Card] History
    deriving Show

usedCards :: ObservedGameState -> [Card]
usedCards (PreGift p0cs _) = p0cs
usedCards (PostGift p0cs p1cs history) = p0cs ++ p1cs ++ concatMap cards history

freeCards :: ObservedGameState -> [Card]
freeCards ogs = deck \\ usedCards ogs

data FullGameState = FullPreGift Player [PlayerState] | FullPostGift [PlayerState] History
    deriving Show

turn :: FullGameState -> Player
turn (FullPreGift p _) = p
turn (FullPostGift _ history) = nextPlayer $ head history

hands :: FullGameState -> [PlayerState]
hands (FullPreGift _ pss) = pss
hands (FullPostGift pss _) = pss

playerState :: Player -> FullGameState -> PlayerState
playerState p fgs = fromJust $ find ((==) p . player) $ hands fgs

isFinalState :: FullGameState -> Bool
isFinalState (FullPreGift _ _) = False
isFinalState (FullPostGift pss _) = all (null . hand) pss

playerScores :: FullGameState -> [(Player, Int)]
playerScores (FullPreGift _ _) = map (,0) players
playerScores (FullPostGift pss _) = [(player ps, score ps) | ps <- pss]

data Move = Deal Player Card | Gift [(Player, [Card])]
    deriving (Show)

data MoveOptions = DealOptions Player [Card] | GiftOptions [(Player, [[Card]])]
    deriving (Show)

moveOptions :: FullGameState -> MoveOptions
moveOptions (FullPreGift _ pss) = GiftOptions [(player ps, gifts $ hand ps) | ps <- pss]
moveOptions (FullPostGift pss history) = DealOptions p choices
    where p = turn (FullPostGift pss history)
          playerCards = hand $ head $ filter ((==) p . player) pss
          choices = moves history playerCards

applyMove :: Move -> FullGameState -> FullGameState
applyMove (Deal _ _) (FullPreGift _ _) = error "FullPreGift only accepts Gift moves"
applyMove (Gift _) (FullPostGift _ _) = error "FullPostGift only accepts Deal moves"
applyMove (Gift playerGifts) (FullPreGift starting pss) = FullPostGift distributedGifts [Table starting []]
    where distributedGifts = [
                PlayerState (player ps) ((hand ps \\ giftGiven) ++ giftTaken) 0
                | ((giftGiven, giftTaken), ps) <- zip (zip sortedGifts (rotate $ rotate $ rotate sortedGifts)) sortedPSs
            ]
          sortedGifts = map snd $ sortOn fst playerGifts
          sortedPSs = sortOn player pss

applyMove (Deal p card) (FullPostGift pss history)
        | p /= turn (FullPostGift pss history) = error "player out of turn"
        | otherwise = FullPostGift (updateScore (updateHistory card) $ popCard card pss) (updateHistory card)
    where popCard c = map (\ps -> PlayerState (player ps) (filter (\_c -> p /= player ps || c /= _c) $ hand ps) (score ps))

          updateScore newHistory = map (\ps -> PlayerState (player ps) (hand ps) (score ps + if collector lastTable == Just (player ps) then currentScore lastTable else 0))
            where lastTable = newHistory !! if isLast then 1 else 0

          updateHistory c
            | isLast = Table (collect newTable) [] : newTable : drop 1 history
            | otherwise = newTable : drop 1 history
            where newTable = (\(Table _p cs) -> Table _p (cs++[c])) (head history)

          isLast = length (cards (head history)) == 3

          currentScore updatedTable
            | isLast = tableScore updatedTable
            | otherwise = 0

          collector updatedTable = if isLast then Just $ collect updatedTable else Nothing

observePlayer :: Player -> Player -> Player
observePlayer observer targetPlayer = toEnum ((fromEnum targetPlayer - fromEnum observer) `mod` 4)

observeFullGameState :: Player -> FullGameState -> FullGameState -> ObservedGameState
observeFullGameState _ (FullPostGift _ _) _ = error "starting state must be FullPreGift"
observeFullGameState observer _ (FullPreGift p pss) =
        PreGift (hand $ head $ filter ((==) observer . player) pss) (observePlayer observer p)
observeFullGameState observer (FullPreGift _ initialPSs) (FullPostGift currentPSs history) =
        PostGift observerCards giftCards observedHistory
    where observerCards = playerCards observer currentPSs
          initialObserverCards = playerCards observer initialPSs

          nextPlayerCards = playerCards (next observer) currentPSs

          giftCards = nextPlayerCards \\ (nextPlayerCards \\ initialObserverCards)

          observedHistory = map (\(Table p cs) -> Table (observePlayer observer p) cs) history

          playerCards p = hand . head . filter ((==) p . player)

generateRandomFullGameState :: ObservedGameState -> RVar FullGameState
generateRandomFullGameState (PreGift p0cs p) = do
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

generateRandomFullGameState (PostGift p0cs p1cs history) = fmap fromJust $ repeatUntil isJust $ do
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
        playerDomain p = filter (\c -> suit c `notElem` missingSuits p history && c `notElem` missingLikhas p history)

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
distributeFreeCards domains
    -- if final game state return empty hands
    | all (\(_, hnum, _) -> hnum == 0) domains = return $ Just [(p,[]) | (p,_,_) <- domains]
    | otherwise = do
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

missingLikhas :: Player -> History -> [Card]
missingLikhas p = concatMap (missingLikhaFromTable p)

missingLikhaFromTable :: Player -> Table -> [Card]
missingLikhaFromTable p (Table start cs)
    | null playerCard = []
    | not (null likhasCanBeCollectedByOthers) && not likhaPlayed = likhasCanBeCollectedByOthers
    | otherwise = []
    where playersCards = zip (iterate next start) cs
          tableSuit = suit $ head cs

          cardsBeforePlayer = map snd $ takeWhile ((/= p) . fst) playersCards

          playerCard = map snd $ filter ((== p) . fst) playersCards
          playerSuit = suit $ head playerCard

          likhasCanBeCollectedByOthers = filter (
            \likha -> collect (pushToTable likha (Table start cardsBeforePlayer)) /= p && likhaPlayable likha
           ) likhas
          
          likhaPlayable likha = tableSuit /= playerSuit || suit likha == tableSuit
            
          likhaPlayed = head playerCard `elem` likhasCanBeCollectedByOthers


missingSuits :: Player -> History -> [Suit]
missingSuits p = mapMaybe (missingFromTable p)

missingFromTable :: Player -> Table -> Maybe Suit
missingFromTable p (Table start cs) = do
    let playerSuits = zip (iterate next start) (map suit cs)
    tableSuit <- snd <$> if not (null playerSuits) then Just (head playerSuits) else Nothing
    playerSuit <- snd <$> find ((==) p . fst) playerSuits
    if playerSuit /= tableSuit then Just tableSuit else Nothing

