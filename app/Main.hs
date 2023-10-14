module Main (main) where

import LikhaAgent (runLikhaGame, randomLikhaAgent, LikhaAgent)
import LikhaMinimax (MinimaxParams(..), monteCarloBestMove, MinimaxAlgorithm(..))

import System.Random( newStdGen, StdGen )
import Data.RVar (sampleStateRVar)
import Control.Monad.State (State, evalState, replicateM)
import LikhaGame (Player (..))
import LikhaGameState (freeCards)
import GHC.Float (int2Float, powerFloat)

import System.IO(hFlush, stdout)

main :: IO ()
main = do
    let heuristicAgent = ("heuristic" , monteCarloBestMove (const $ MinimaxParams {
            algorithm = AlphaBeta,
            monteCarloSamples = 1,
            maxTreeDepthPreGift = 1,
            maxTreeDepthPostGift = 2,
            maxTreeWidth = 1,
            maxMatrixDimensions = 1
        }))
    let smartAgent = ("smart" , monteCarloBestMove (const $ MinimaxParams {
            algorithm = AlphaBeta,
            monteCarloSamples = 1,
            maxTreeDepthPreGift = 1,
            maxTreeDepthPostGift = 6,
            maxTreeWidth = 2,
            maxMatrixDimensions = 1
        }))
    let geniusAgent = ("genius" , monteCarloBestMove (\ogs -> MinimaxParams {
            algorithm = AlphaBeta,
            monteCarloSamples = max (length $ freeCards ogs) 1,
            maxTreeDepthPreGift = 1,
            maxTreeDepthPostGift = 8,
            maxTreeWidth = 5,
            maxMatrixDimensions = 1
        }))
    let randomAgent = ("random", randomLikhaAgent)

    simulateNGames 100 [heuristicAgent, randomAgent, heuristicAgent, randomAgent]

    simulateNGames 100 [smartAgent, heuristicAgent, smartAgent, heuristicAgent]

    simulateNGames 100 [geniusAgent, smartAgent, geniusAgent, smartAgent]

    return ()

simulateNGames :: Int -> [(String, LikhaAgent)] -> IO ()
simulateNGames n agents = do
    putStrLn $ "Simulating " ++ show n ++ " instances of likha with players: " ++ show (map fst agents)

    src <- newStdGen
    let results = evalState (replicateM n $ simulateGame (map snd agents)) src

    progressBar n results

    putStr "Average scores: "
    let average xss = map (/int2Float (length xss)) $ foldl (zipWith (+)) [0,0,0,0] xss
    let averageScores = average (map (map int2Float) results)
    print averageScores

    putStr "Std deviation: "
    print $ map sqrt $ average $ map (map (powerFloat 2) . zipWith (-) averageScores . map int2Float) results

    let wins = length $ filter (Player0 ==) $ map winner results

    putStrLn (show ((wins * 100) `div` n) ++ "% won")

    return ()

progressBar :: Int -> [a] -> IO ()
progressBar _ xs = do
    let progress = do
            putStr "."
            hFlush stdout

    mapM_ (`seq` progress) xs
    putStrLn ""

winner :: [Int] -> Player
winner scores = if max0 < max1 then Player0 else Player1
    where max0 = max (head scores) (scores !! 2)
          max1 = max (scores !! 1) (scores !! 3)

simulateGame :: [LikhaAgent] -> State StdGen [Int]
simulateGame agents = do
    sampleStateRVar $ runLikhaGame agents

