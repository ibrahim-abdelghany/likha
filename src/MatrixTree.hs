{-# LANGUAGE InstanceSigs #-}
module MatrixTree
(
  MatrixTree(..),
  value,
  offspring,
  iterateMatrixTree,
  mapMatrix,
  prune,
  Turn(..),
  minimax,
  alphaBetaMinimax
) where

data MatrixTree a = TreeNode a [MatrixTree a] | MatrixNode a [[[[MatrixTree a]]]]
    deriving (Show, Eq)

value :: MatrixTree a -> a
value (TreeNode x _) = x
value (MatrixNode x _) = x

offspring :: MatrixTree a -> [MatrixTree a]
offspring (TreeNode _ children) = children
offspring (MatrixNode _ cells) = concatMap (concatMap concat) cells

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
prune depth width dim = pruneWidth width . pruneDepth depth . pruneMatrix dim

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
        MatrixNode (x, if null cells then heuristic x else minimaxMatrix $ mapMatrix (snd . value) minimaxCells) minimaxCells
    where minimaxCells = mapMatrix (minimax turn heuristic) cells
minimax turn heuristic (TreeNode x children) =
        TreeNode (x, if null children then heuristic x else minOrMax $ map (snd . value) minimaxChildren) minimaxChildren
    where minimaxChildren = map (minimax turn heuristic) children
          minOrMax = case turn x of
            Min -> minimum
            Max -> maximum

minimaxMatrix :: Ord b => [[[[b]]]] ->  b
minimaxMatrix matrix = minimum $ map (maximum . map (minimum . map maximum)) matrix

alphaBetaMinimax :: Ord b => (a -> Turn) -> (a -> b) -> MatrixTree a -> MatrixTree (a, b)
alphaBetaMinimax turn heuristic (MatrixNode x cells) =
        MatrixNode (x, if null cells then heuristic x else minimaxMatrix $ mapMatrix (snd . value) minimaxCells) minimaxCells
    where minimaxCells = mapMatrix (alphaBetaMinimax turn heuristic) cells
alphaBetaMinimax turn heuristic (TreeNode x children)
        | turn x == Min && all ((==) Max . turn . value) children = minimizeMaximize (TreeNode x children)
        | turn x == Max && all ((==) Min . turn . value) children = maximizeMinimize (TreeNode x children)
        | otherwise = TreeNode (x, if null children then heuristic x else minOrMax $ map (snd . value) minimaxChildren) minimaxChildren
    where minimaxChildren = map (alphaBetaMinimax turn heuristic) children
          minOrMax = case turn x of
            Min -> minimum
            Max -> maximum

          minimizeMaximize (MatrixNode _ _) = error "unexpected input"
          minimizeMaximize (TreeNode x_ []) = TreeNode (x_, heuristic x_) []
          minimizeMaximize (TreeNode x_ (c:cs)) = TreeNode (x_, minimum $ map (snd . value) maximizedChildren) maximizedChildren
            where maximizedChildren = pruneLargerMax (alphaBetaMinimax turn heuristic c) (map (alphaBetaMinimax turn heuristic) cs)

                  pruneLargerMax :: Ord b => MatrixTree (a, b) -> [MatrixTree (a, b)] -> [MatrixTree (a, b)]
                  pruneLargerMax first rest = first : omitLargeMax (snd $ value first) rest

                  omitLargeMax :: Ord b => b -> [MatrixTree (a, b)] -> [MatrixTree (a, b)]
                  omitLargeMax _ [] = []
                  omitLargeMax _ ((MatrixNode {}):_) = error "fail"
                  omitLargeMax smallestMax ((TreeNode (cx,cv) ccs):rest)
                    | all ((<= smallestMax) . snd . value) ccs = TreeNode (cx,cv) ccs: omitLargeMax cv rest
                    | otherwise = omitLargeMax smallestMax rest

          maximizeMinimize (MatrixNode _ _) = error "unexpected input"
          maximizeMinimize (TreeNode x_ []) = TreeNode (x_, heuristic x_) []
          maximizeMinimize (TreeNode x_ (c:cs)) = TreeNode (x_, maximum $ map (snd . value) minimizedChildren) minimizedChildren
            where minimizedChildren = pruneSmallerMin (alphaBetaMinimax turn heuristic c) (map (alphaBetaMinimax turn heuristic) cs)

                  pruneSmallerMin :: Ord b => MatrixTree (a, b) -> [MatrixTree (a, b)] -> [MatrixTree (a, b)]
                  pruneSmallerMin first rest = first : omitSmallerMin (snd $ value first) rest

                  omitSmallerMin :: Ord b => b -> [MatrixTree (a, b)] -> [MatrixTree (a, b)]
                  omitSmallerMin _ [] = []
                  omitSmallerMin _ ((MatrixNode {}):_) = error "fail"
                  omitSmallerMin largestMin ((TreeNode (cx,cv) ccs):rest)
                    | all ((>= largestMin) . snd . value) ccs = TreeNode (cx,cv) ccs: omitSmallerMin cv rest
                    | otherwise = omitSmallerMin largestMin rest