module ListUtils (
    pick,
    rotate
) where

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick n (x:xs) = [x:ps | ps <- pick (n-1) xs] ++ pick n xs

rotate :: [a] -> [a]
rotate xs = tail xs ++ [head xs]