module Main (main) where

import LikhaAgent (runLikhaGame, randomLikhaAgent, LikhaAgent)
import LikhaMinimax (MinimaxParams(..), monteCarloBestMove)

import System.Random( newStdGen, StdGen )
import Data.RVar (sampleStateRVar)
import Control.Monad.State (State, evalState, replicateM)
import LikhaGame (Player (..))

main :: IO ()
main = do
    let heuristicAgent = ("heuristic" , monteCarloBestMove (MinimaxParams {
            monteCarloSamples = 1,
            maxTreeDepthPreGift = 1,
            maxTreeDepthPostGift = 2,
            maxTreeWidth = 1,
            maxMatrixDimensions = 1
        }))
    let twoOptionsOneTableLookahead = ("twoOptionsOneTableLookahead" , monteCarloBestMove (MinimaxParams {
            monteCarloSamples = 1,
            maxTreeDepthPreGift = 1+4,
            maxTreeDepthPostGift = 2+4,
            maxTreeWidth = 2,
            maxMatrixDimensions = 1
        }))
    let twoOptionsTwoTableLookahead = ("twoOptionsOneTableLookahead10Samples" , monteCarloBestMove (MinimaxParams {
            monteCarloSamples = 1,
            maxTreeDepthPreGift = 1+4*2,
            maxTreeDepthPostGift = 2+4*2,
            maxTreeWidth = 4,
            maxMatrixDimensions = 1
        }))
    let randomAgent = ("random", randomLikhaAgent)

    simulateNGames 100 [heuristicAgent, randomAgent, heuristicAgent, randomAgent]

    simulateNGames 100 [twoOptionsOneTableLookahead, heuristicAgent, twoOptionsOneTableLookahead, heuristicAgent]

    simulateNGames 100 [twoOptionsTwoTableLookahead, heuristicAgent, twoOptionsTwoTableLookahead, heuristicAgent]

    return ()

simulateNGames :: Int -> [(String, LikhaAgent)] -> IO ()
simulateNGames n agents = do
    putStrLn $ "Simulating " ++ show n ++ " instances of likha with players: " ++ show (map fst agents)

    let results = fmap (length . filter (Player0 ==) . map winner) (replicateM 100 $ simulateGame (map snd agents))

    src <- newStdGen

    putStrLn (show ((evalState results src * 100) `div`  n ) ++ "% won")

    return ()

winner :: [Int] -> Player
winner scores = if max0 < max1 then Player0 else Player1
    where max0 = max (head scores) (scores !! 2)
          max1 = max (scores !! 1) (scores !! 3)

simulateGame :: [LikhaAgent] -> State StdGen [Int]
simulateGame agents = do
    sampleStateRVar $ runLikhaGame agents

