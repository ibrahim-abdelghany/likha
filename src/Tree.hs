{-# LANGUAGE InstanceSigs #-}
module Tree (
    Tree(..),
    iterateTree,
    pruneWidth,
    pruneDepth,
    sortChildrenOn
) where

import Data.List (sortOn)

data Tree a = Node a [Tree a]
    deriving (Show, Eq)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Node x subtrees) = Node (g x) $ map (fmap g) subtrees

iterateTree :: (a -> [a]) -> a -> Tree a
iterateTree f root = Node root $ map (iterateTree f) (f root)

pruneWidth :: Int -> Tree a -> Tree a
pruneWidth w (Node a children) = Node a (take w $ map (pruneWidth w) children)

pruneDepth :: Int -> Tree a -> Tree a
pruneDepth 0 (Node a _) = Node a []
pruneDepth d (Node a children) = Node a $ map (pruneDepth (d-1)) children

sortChildrenOn :: Ord b => (a -> b) -> Tree a -> Tree a
sortChildrenOn g (Node root children) = Node root $ sortOn (g . \(Node x _) -> x) children