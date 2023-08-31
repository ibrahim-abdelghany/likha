module MatrixTreeSpec (spec) where

import Test.Hspec ( describe, it, shouldSatisfy, Spec, shouldBe )

import MatrixTree (MatrixTree(..), iterateMatrixTree, mapMatrix, prune, value, Turn (..), minimax)

spec :: Spec
spec = do
    describe "prune" $ do
        it "prunes matrix dimensions" $
            prune 3 2 2 infiniteMatrixTree `shouldSatisfy` \mt -> matrixDimensions mt == 2
        it "prunes tree depth" $
            prune 3 2 2 infiniteMatrixTree `shouldSatisfy` \mt -> treeDepth mt == 3
        it "prunes tree width" $
            prune 3 2 2 infiniteMatrixTree `shouldSatisfy` \mt -> treeWidth mt == 2
    describe "iterateMatrixTree" $ do
        it "generates tree from function" $
            iterateMatrixTree (\n -> Left (replicate n (n-1))) 2 `shouldBe` TreeNode 2 [TreeNode 1 [TreeNode 0 []], TreeNode 1 [TreeNode 0 []]]
        it "generates matrix from function" $
            iterateMatrixTree (\n -> if n == 0 then Right [] else Right [[[[n-1]]]]) (2 :: Int) `shouldBe` MatrixNode 2 [[[[MatrixNode 1 [[[[MatrixNode 0 []]]]]]]]]
    describe "minimax" $ do
        it "solves matrix minimax correctly" $
            minimax (\i -> if even i then Min else Max) id dummyMatrixGame `shouldSatisfy` ((==6) . snd . value)
        it "solves tree minimax correctly" $
            minimax (\i -> if even i then Min else Max) id dummyTreeGame `shouldSatisfy` ((==20) . snd . value)

infiniteMatrixTree :: MatrixTree Int
infiniteMatrixTree = iterateMatrixTree infiniteIterator 0
    where infiniteIterator n | even n = Right [[[map ((+1) . (*2)) [a,b,c] | a <-[0..]] | b <- [0..]] | c<-[0..]]
                             | otherwise = Left $ map (*2) [0..]

treeDepth :: MatrixTree a -> Int
treeDepth (TreeNode _ children)
    | null children = 1
    | otherwise = 1 + maximum (map treeDepth children)
treeDepth (MatrixNode _ cells)
    | null cells = 0
    | otherwise = maximum $ flattenMatrix $ mapMatrix treeDepth cells

treeWidth :: MatrixTree a -> Int
treeWidth (TreeNode _ children)
    | null children = 0
    | otherwise = max (length children) (maximum $ map treeWidth children)
treeWidth (MatrixNode _ cells)
    | null cells = 0
    | otherwise = maximum $ flattenMatrix $ mapMatrix treeWidth cells

matrixDimensions :: MatrixTree a -> Int
matrixDimensions (TreeNode _ children)
    | null children = 0
    | otherwise = maximum $ map matrixDimensions children
matrixDimensions (MatrixNode _ cells)
    | null cells = 0
    | otherwise = max (d4 cells) (maximum $ flattenMatrix $ mapMatrix matrixDimensions cells)
    where d4 [] = 0
          d4 xs = max (length xs) (maximum $ map d3 xs)
          d3 [] = 0
          d3 xs = max (length xs) (maximum $ map d2 xs)
          d2 [] = 0
          d2 xs = max (length xs) (maximum $ map d1 xs)
          d1 = length

flattenMatrix :: [[[[a]]]] -> [a]
flattenMatrix = concatMap (concatMap concat)

dummyTreeGame :: MatrixTree Int
dummyTreeGame = TreeNode 0 [
        TreeNode 1 [
            TreeNode 2 [
                TreeNode 5 [
                    TreeNode 10 [],
                    TreeNode 12 []
                ],
                TreeNode 7 [
                    TreeNode 14 [],
                    TreeNode 16 []
                ]
            ], TreeNode 4 [
                TreeNode 9 [
                    TreeNode 18 [],
                    TreeNode 20 []
                ],
                TreeNode 11 [
                    TreeNode 22 [],
                    TreeNode 24 []
                ]
            ]
        ],
        TreeNode 3 [
            TreeNode 6 [
                TreeNode 13 [
                    TreeNode 26 [],
                    TreeNode 28 []
                ],
                TreeNode 15 [
                    TreeNode 30 [],
                    TreeNode 32 []
                ]
            ], TreeNode 8 [
                TreeNode 17 [
                    TreeNode 34 [],
                    TreeNode 36 []
                ],
                TreeNode 19 [
                    TreeNode 38 [],
                    TreeNode 40 []
                ]
            ]
        ]
    ]

dummyMatrixGame :: MatrixTree Int
dummyMatrixGame = MatrixNode 0 [
        [
            [
                [MatrixNode 1 [], MatrixNode 2 []],
                [MatrixNode 3 [], MatrixNode 4 []]
            ], [
                [MatrixNode 5 [], MatrixNode 6 []],
                [MatrixNode 7 [], MatrixNode 8 []]
            ]
        ],
        [
            [
                [MatrixNode 9 [], MatrixNode 10 []],
                [MatrixNode 11 [], MatrixNode 12 []]
            ], [
                [MatrixNode 13 [], MatrixNode 14 []],
                [MatrixNode 15 [], MatrixNode 16 []]
            ]
        ]
    ]
