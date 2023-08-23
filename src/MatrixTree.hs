{-# LANGUAGE InstanceSigs #-}
module MatrixTree
(
  MatrixTree(..),
  value,
  iterateMatrixTree,
  mapMatrix,
  prune,
  Turn(..),
  minimax
) where

import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)

data MatrixTree a = TreeNode a [MatrixTree a] | MatrixNode a [[[[MatrixTree a]]]]
    deriving (Show, Eq)

value :: MatrixTree a -> a
value (TreeNode x _) = x
value (MatrixNode x _) = x

instance Functor MatrixTree where
    fmap :: (a -> b) -> MatrixTree a -> MatrixTree b
    fmap g (TreeNode x subtrees) = TreeNode (g x) $ map (fmap g) subtrees
    fmap g (MatrixNode x cells) = MatrixNode (g x) $ mapMatrix (fmap g) cells

mapMatrix :: (a -> b) -> [[[[a]]]] -> [[[[b]]]]
mapMatrix f = map (map (map (map f)))

iterateMatrixTree :: (a -> Either [a] [[[[a]]]]) -> a -> MatrixTree a
iterateMatrixTree f root = case f root of
    Left children -> TreeNode root $ map (iterateMatrixTree f) children
    Right cells -> MatrixNode root $ mapMatrix (iterateMatrixTree f) cells

prune :: Int -> Int -> Int -> MatrixTree a -> MatrixTree a
prune width depth dim = pruneWidth width . pruneDepth depth . pruneMatrix dim

pruneWidth :: Int -> MatrixTree a -> MatrixTree a
pruneWidth w (TreeNode x children) = TreeNode x (take w $ map (pruneWidth w) children)
pruneWidth w (MatrixNode x cells) = MatrixNode x (mapMatrix (pruneWidth w) cells)

pruneDepth :: Int -> MatrixTree a -> MatrixTree a
pruneDepth d _ | d <= 0 = error "depth must be positive"
pruneDepth 1 (TreeNode x _) = TreeNode x []
pruneDepth d (TreeNode x children) = TreeNode x $ map (pruneDepth (d-1)) children
pruneDepth d (MatrixNode x cells) = MatrixNode x $ mapMatrix (pruneDepth d) cells

pruneMatrix :: Int -> MatrixTree a -> MatrixTree a
pruneMatrix n (MatrixNode x cells) = MatrixNode x $ take n $ map (take n . map (take n . map (take n))) $ mapMatrix (pruneMatrix n) cells
pruneMatrix n (TreeNode x children) = TreeNode x $ map (pruneMatrix n) children

data Turn = Min | Max
    deriving (Show, Eq)

minimax ::  Ord b => (a -> Turn) -> (a -> b) -> MatrixTree a -> MatrixTree (a, b)
minimax turn heuristic (MatrixNode x cells) = 
        MatrixNode (x, if null cells then heuristic x else minimaxMatrix $ mapMatrix value minimaxCells) minimaxCells
    where minimaxCells = mapMatrix (minimax turn heuristic) cells
minimax turn heuristic (TreeNode x children) = 
    case turn x of
        Min -> TreeNode (x, if null children then heuristic x else minimum $ map (snd . value) minimaxChildren) minimaxChildren
        Max -> TreeNode (x, if null children then heuristic x else maximum $ map (snd . value) minimaxChildren) minimaxChildren
    where minimaxChildren = map (minimax turn heuristic) children

minimaxMatrix :: Ord b => [[[[(a, b)]]]] ->  b
minimaxMatrix matrix = snd $ minState $ map (maxState . map (minState . map maxState)) matrix
    where compareScore ::  Ord b => (a, b) -> (a, b) -> Ordering
          compareScore = comparing snd

          maxState :: Ord b => [(a, b)] -> (a, b)
          maxState = maximumBy compareScore

          minState :: Ord b => [(a, b)] -> (a, b)
          minState = minimumBy compareScore
