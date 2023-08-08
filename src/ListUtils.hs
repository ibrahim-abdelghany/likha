module ListUtils (
    pick,
    outerProduct,
    rotate
) where

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick n (x:xs) = [x:ps | ps <- pick (n-1) xs] ++ pick n xs

outerProduct :: [[a]] -> [a] -> [[a]]
outerProduct xss ys = [xs ++ [y] | xs <- xss, y <- ys]

rotate :: [a] -> [a]
rotate xs = tail xs ++ [head xs]