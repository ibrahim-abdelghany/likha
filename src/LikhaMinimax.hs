module LikhaMinimax
(
  monteCarloBestMove
) where

import Data.Random ( RVar )
import Data.List ((\\), sortOn, sort, group)
import Data.Foldable ( maximumBy )
import Data.Ord (comparing)
import Control.Monad (replicateM)

import ListUtils (rotate)

import Cards (Card(..), suit)
import LikhaGame (Player, Table(..), nextPlayer, collect, moves, gifts, tableScore)
import LikhaGameState (PlayerState(..), ObservedGameState, FullGameState (..), generateFullGameState, turn, hands)
import LikhaGameHeuristics (giftHeuristic, gameStateHeuristic)

import MatrixTree (MatrixTree(..), value, offspring, Turn (..), iterateMatrixTree, prune, minimax)

data MinimaxParams = MinimaxParams {
      monteCarloSamples :: Int,
      maxTreeDepthPreGift :: Int,
      maxTreeDepthPostGift :: Int,
      maxTreeWidth :: Int,
      maxMatrixDimensions :: Int
  }
  deriving (Show, Eq)

monteCarloBestMove :: MinimaxParams -> ObservedGameState -> RVar [Card]
monteCarloBestMove params observedGameState = do
    sampledGameStates <- replicateM (monteCarloSamples params) $ generateFullGameState observedGameState
    let bestMoves = map (minimaxBestMove params) sampledGameStates
    return $ head $ maximumBy (comparing length) $ group $ sort bestMoves

minimaxBestMove :: MinimaxParams -> FullGameState -> [Card]
minimaxBestMove params initialState = 
        bestMove $ 
        minimax (playerGoal . turn) gameStateHeuristic $ 
        prune maxDepth (maxTreeWidth params) (maxMatrixDimensions params) $ 
        iterateMatrixTree nextStates initialState
    where maxDepth = case initialState of 
                        (FullPreGift _ _) -> maxTreeDepthPreGift params
                        (FullPostGift _ _) -> maxTreeDepthPostGift params

bestMove :: Eq b => MatrixTree (FullGameState, b) -> [Card]
bestMove parent = playingHand parentGS \\ playingHand bestChild
    where parentGS = fst $ value parent
          parentScore = snd $ value parent
          bestChild = fst $ head $ filter ((==) parentScore . snd) $ map value $ offspring parent
          playingHand gs = hand $ head $ filter ((==) (turn parentGS) . player) $ hands gs

gameScore :: [(Player, Float)] -> Float
gameScore playerScores = player0s - player1s
  where player0s = maximum $ map snd $ filter ((== Min) . playerGoal . fst) playerScores
        player1s = maximum $ map snd $ filter ((== Max) . playerGoal . fst) playerScores

playerGoal :: Player -> Turn
playerGoal p = if even $ fromEnum p then Min else Max

playerCost :: Player -> Float -> Float
playerCost p s = if playerGoal p == Min then s else -s

nextStates :: FullGameState -> Either [FullGameState] [[[[FullGameState]]]]
nextStates (FullPreGift p playerStates) = Right [[[[
        generateGameState [gifts0, gifts1, gifts2, gifts3]
        | gifts3 <- giftChoicesPerPlayer !! 3]
        | gifts2 <- giftChoicesPerPlayer !! 2]
        | gifts1 <- giftChoicesPerPlayer !! 1]
        | gifts0 <- head giftChoicesPerPlayer]
    where giftChoicesPerPlayer = [sortOn (giftHeuristic $ hand ps) $ gifts $ hand ps | ps <- sortOn player playerStates]
          generateGameState gs = FullPostGift (distributeGifts gs) [Table p []]
          distributeGifts gs = [PlayerState (player ps) ((hand ps \\ giftGiven) ++ giftTaken ) 0 | ((giftGiven, giftTaken), ps) <- zip (zip gs (rotate gs)) $ sortOn player playerStates]
nextStates (FullPostGift playerStates history) = Left $ 
        sortOn (playerCost tablePlayer . gameScore . gameStateHeuristic)
        [FullPostGift (updateScore (updateHistory c) $ popCard c playerStates) (updateHistory c) | c <- choices]
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
