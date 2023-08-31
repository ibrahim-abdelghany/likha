{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module LikhaAgent (
    LikhaAgent,
    randomLikhaAgent,
    runLikhaGame
) where

import Data.RVar (RVar)
import Data.Random.List (shuffle, randomElement)
import Data.List (singleton, sortOn)
import Control.Monad (zipWithM)

import Cards (Card, deck)
import LikhaGame (moves, players)
import LikhaGameState (ObservedGameState(..), FullGameState (..), PlayerState (..), turn, observeFullGameState, Move (..), applyMove, isFinalState, playerScores)

type LikhaAgent = ObservedGameState -> RVar [Card]

randomLikhaAgent :: ObservedGameState -> RVar [Card]
randomLikhaAgent (PreGift p0cs _) = do
    shuffledCards <- shuffle p0cs
    return $ take 3 shuffledCards
randomLikhaAgent (PostGift p0cs _ history) = do
    c <- randomElement $ moves history p0cs
    return $ singleton c

runLikhaGame :: [LikhaAgent] -> RVar [Int]
runLikhaGame agents = do
    shuffledDeck <- shuffle deck
    let playerCards = [
                take 13 shuffledDeck,
                take 13 $ drop 13 shuffledDeck,
                take 13 $ drop 26 shuffledDeck,
                drop 39 shuffledDeck
            ]

    starting <- randomElement players

    let initialState = FullPreGift starting $ zipWith (\ p cs -> PlayerState p cs 0) players playerCards

    stateSequence <- iterateUntilM isFinalState (getNextState agents initialState) initialState

    return $ map snd $ sortOn fst $ playerScores $ last stateSequence

iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m [a]
iterateUntilM isFinal next start =
    if isFinal start then do
        return [start]
    else do
        state <- next start
        rest <- iterateUntilM isFinal next state
        return (start:rest)

getNextState :: [LikhaAgent] -> FullGameState -> FullGameState -> RVar FullGameState
getNextState agents initialState currentState = case currentState of
        FullPreGift _ _ -> getPreGiftMoves
        FullPostGift _ _ -> getPostGiftMove
    where getPreGiftMoves = do
            let getMoves agent p = agent $ observeFullGameState p initialState currentState

            agentMoves <- zipWithM getMoves agents players

            return $ applyMove (Gift (zip players agentMoves)) currentState

          getPostGiftMove = do
            let p = turn currentState
            let agent = agents !! fromEnum p
            let observedGS = observeFullGameState p initialState currentState

            move <- agent observedGS

            return $ applyMove (Deal p $ head move) currentState
