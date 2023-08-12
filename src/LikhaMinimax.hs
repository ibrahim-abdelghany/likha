module LikhaMinimax
(
  minimax
) where

import Data.Random ( RVar )
import Data.List ((\\), sortOn)
import Control.Monad (replicateM)

import ListUtils (rotate)
import Tree (iterateTree, sortChildrenOn, pruneWidth, pruneDepth)

import Cards (Card(..), suit)
import LikhaGame (Table(..), nextPlayer, collect, moves, gifts, tableScore, Player)
import LikhaGameState (PlayerState(..), ObservedGameState, FullGameState (..), generateFullGameState, currentPlayer)
import LikhaGameHeuristics (giftHeuristic, gameStateHeuristic)

minimax :: ObservedGameState -> RVar [Card]
minimax observedGameState = do
  sampledGameStates <- replicateM 100 $ generateFullGameState observedGameState
  let sampledGameTrees = map pureMinimax sampledGameStates
  return []

pureMinimax :: FullGameState -> [Card]
pureMinimax fullGameState = []
  where gameTree = pruneDepth 5 $ pruneWidth 5 $ sortChildrenOn minOrMax (augmentScore <$> iterateTree nextGameStates fullGameState)
        augmentScore gs = (gs, gameScore $ gameStateHeuristic gs)
        minOrMax (gs, s) = if even (fromEnum $ currentPlayer gs) then s else -s

gameScore :: [(Player, Float)] -> Float
gameScore playerScores = player0s - player1s
  where player0s = maximum $ map snd $ filter (even . fromEnum . fst) playerScores
        player1s = maximum $ map snd $ filter (odd . fromEnum . fst) playerScores

-- contract:
--     history never empty, initially contains table with starting player and no cards
nextGameStates :: FullGameState -> [FullGameState]
nextGameStates (FullPreGift p playerStates) = map generateGameState giftChoices
    where giftChoicesPerPlayer = [take 3 $ sortOn (giftHeuristic $ hand ps) $ gifts $ hand ps | ps <- sortOn player playerStates]
          giftChoices = sequence giftChoicesPerPlayer
          generateGameState gs = FullPostGift (distributeGifts gs) [Table p []]
          distributeGifts gs = [PlayerState (player ps) ((hand ps \\ giftGiven) ++ giftTaken ) 0 | ((giftGiven, giftTaken), ps) <- zip (zip gs (rotate gs)) $ sortOn player playerStates]

nextGameStates (FullPostGift playerStates history) =
        [ FullPostGift (updateScore (updateHistory c) $ popCard c playerStates) (updateHistory c) | c <- choices ]
    where popCard c = map (\ps -> PlayerState (player ps) (filter (\_c -> tablePlayer /= player ps && c /= _c) $ hand ps) (score ps))

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

          choices
            | null (cards $ head history) = playerCards
            | otherwise = moves (suit $ head $ cards $ head history) playerCards
            where playerCards = hand $ head $ filter ((==) tablePlayer . player) playerStates

          tablePlayer = nextPlayer $ head history

