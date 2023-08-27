module LikhaMinimax
(
  MinimaxParams(..),
  monteCarloBestMove,
  nextStates
) where

import Data.Random ( RVar )
import Data.List ((\\), sortOn, sort, group)
import Data.Foldable ( maximumBy )
import Data.Ord (comparing)
import Control.Monad (replicateM)

import Cards (Card(..))
import LikhaGame (Player (..))
import LikhaGameState (PlayerState(..), ObservedGameState, FullGameState (..), generateRandomFullGameState, turn, hands, MoveOptions (..), moveOptions, playerState, applyMove, Move (Deal, Gift))
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
    sampledGameStates <- replicateM (monteCarloSamples params) $ generateRandomFullGameState observedGameState
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
nextStates fgs = case moveOptions fgs of
    GiftOptions giftOptions -> Right [[[[
      applyMove (Gift [(Player0, g0), (Player1, g1), (Player2, g2), (Player3, g3)]) fgs
          | g0 <- head sortedGiftOptions]
          | g1 <- sortedGiftOptions !! 1]
          | g2 <- sortedGiftOptions !! 2]
          | g3 <- sortedGiftOptions !! 3]
      where sortedGiftOptions = map snd $ sortOn fst $ map (uncurry sortGifts) giftOptions
            sortGifts p gs = (p, sortOn (giftHeuristic (hand $ playerState p fgs)) gs)
    DealOptions p cardOptions -> Left $ sortOn (playerCost p . gameScore . gameStateHeuristic) $ [applyMove (Deal p c) fgs | c <- cardOptions]
