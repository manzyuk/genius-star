module Main where

import Data.List (find, nub)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import System.Environment (getArgs)

data Direction = X | Y | Z deriving (Eq, Ord, Show)

neighbour :: Direction -> Int -> Maybe Int

neighbour X 1 = Nothing
neighbour Y 1 = Nothing
neighbour Z 1 = Just 3

neighbour X 2 = Just 3
neighbour Y 2 = Nothing
neighbour Z 2 = Just 9

neighbour X 3 = Just 2
neighbour Y 3 = Just 4
neighbour Z 3 = Just 1

neighbour X 4 = Nothing
neighbour Y 4 = Just 3
neighbour Z 4 = Just 11

neighbour X 5 = Nothing
neighbour Y 5 = Just 6
neighbour Z 5 = Nothing

neighbour X 6 = Just 7
neighbour Y 6 = Just 5
neighbour Z 6 = Just 16

neighbour X 7 = Just 6
neighbour Y 7 = Just 8
neighbour Z 7 = Nothing

neighbour X 8 = Just 9
neighbour Y 8 = Just 7
neighbour Z 8 = Just 18

neighbour X 9 = Just 8
neighbour Y 9 = Just 10
neighbour Z 9 = Just 2

neighbour X 10 = Just 11
neighbour Y 10 = Just 9
neighbour Z 10 = Just 20

neighbour X 11 = Just 10
neighbour Y 11 = Just 12
neighbour Z 11 = Just 4

neighbour X 12 = Just 13
neighbour Y 12 = Just 11
neighbour Z 12 = Just 22

neighbour X 13 = Just 12
neighbour Y 13 = Just 14
neighbour Z 13 = Nothing

neighbour X 14 = Just 15
neighbour Y 14 = Just 13
neighbour Z 14 = Just 24

neighbour X 15 = Just 14
neighbour Y 15 = Nothing
neighbour Z 15 = Nothing

neighbour X 16 = Nothing
neighbour Y 16 = Just 17
neighbour Z 16 = Just 6

neighbour X 17 = Just 18
neighbour Y 17 = Just 16
neighbour Z 17 = Just 26

neighbour X 18 = Just 17
neighbour Y 18 = Just 19
neighbour Z 18 = Just 8

neighbour X 19 = Just 20
neighbour Y 19 = Just 18
neighbour Z 19 = Just 28

neighbour X 20 = Just 19
neighbour Y 20 = Just 21
neighbour Z 20 = Just 10

neighbour X 21 = Just 22
neighbour Y 21 = Just 20
neighbour Z 21 = Just 30

neighbour X 22 = Just 21
neighbour Y 22 = Just 23
neighbour Z 22 = Just 12

neighbour X 23 = Just 24
neighbour Y 23 = Just 22
neighbour Z 23 = Just 32

neighbour X 24 = Just 23
neighbour Y 24 = Nothing
neighbour Z 24 = Just 14

neighbour X 25 = Just 26
neighbour Y 25 = Nothing
neighbour Z 25 = Just 35

neighbour X 26 = Just 25
neighbour Y 26 = Just 27
neighbour Z 26 = Just 17

neighbour X 27 = Just 28
neighbour Y 27 = Just 26
neighbour Z 27 = Just 37

neighbour X 28 = Just 27
neighbour Y 28 = Just 29
neighbour Z 28 = Just 19

neighbour X 29 = Just 30
neighbour Y 29 = Just 28
neighbour Z 29 = Just 39

neighbour X 30 = Just 29
neighbour Y 30 = Just 31
neighbour Z 30 = Just 21

neighbour X 31 = Just 32
neighbour Y 31 = Just 30
neighbour Z 31 = Just 41

neighbour X 32 = Just 31
neighbour Y 32 = Just 33
neighbour Z 32 = Just 23

neighbour X 33 = Nothing
neighbour Y 33 = Just 32
neighbour Z 33 = Just 43

neighbour X 34 = Just 35
neighbour Y 34 = Nothing
neighbour Z 34 = Nothing

neighbour X 35 = Just 34
neighbour Y 35 = Just 36
neighbour Z 35 = Just 25

neighbour X 36 = Just 37
neighbour Y 36 = Just 35
neighbour Z 36 = Nothing

neighbour X 37 = Just 36
neighbour Y 37 = Just 38
neighbour Z 37 = Just 27

neighbour X 38 = Just 39
neighbour Y 38 = Just 37
neighbour Z 38 = Just 45

neighbour X 39 = Just 38
neighbour Y 39 = Just 40
neighbour Z 39 = Just 29

neighbour X 40 = Just 41
neighbour Y 40 = Just 39
neighbour Z 40 = Just 47

neighbour X 41 = Just 40
neighbour Y 41 = Just 42
neighbour Z 41 = Just 31

neighbour X 42 = Just 43
neighbour Y 42 = Just 41
neighbour Z 42 = Nothing

neighbour X 43 = Just 42
neighbour Y 43 = Just 44
neighbour Z 43 = Just 33

neighbour X 44 = Nothing
neighbour Y 44 = Just 43
neighbour Z 44 = Nothing

neighbour X 45 = Nothing
neighbour Y 45 = Just 46
neighbour Z 45 = Just 38

neighbour X 46 = Just 47
neighbour Y 46 = Just 45
neighbour Z 46 = Just 48

neighbour X 47 = Just 46
neighbour Y 47 = Nothing
neighbour Z 47 = Just 40

neighbour X 48 = Nothing
neighbour Y 48 = Nothing
neighbour Z 48 = Just 46

neighbour _ n = error $ "invalid number: " ++ show n

type Path = [Direction]
type Shape = [Path]

shape1, shape2, shape3, shape4, shape5,
  shape6, shape7, shape9, shape10, shape11 :: Shape

--  /\
-- /*_\
shape1 = [[]]

--   ____
--  /\  /
-- /*_\/
shape2 = [[], [X]]

--   ____
--  /\* /\
-- /__\/__\
shape3 = [[], [X], [Y]]
shape4 = [[], [X], [Y]]

--    /\
--   /__\
--  /\* /\
-- /__\/__\
shape5 = [[], [X], [Y], [Z]]

--    /\
--   /__\____
--  /\* /\  /
-- /__\/__\/
shape6 = [[], [X], [Y], [Z], [Z, X]]

--   ____
--  /\* /\
-- /__\/__\
--     \  /
--      \/
shape7 = [[], [X], [Y], [X, Z]]

--   ________
--  /\* /\  /
-- /__\/__\/
shape8 = [[], [X], [Y], [Y, X]]

--   ________
--  /\  /\  /\
-- /__\/*_\/__\
shape9 = [[], [X], [Y], [X, Y], [Y, X]]

--   ____
--  /\* /\
-- /__\/__\
-- \  /\  /
--  \/  \/
shape10 = [[], [X], [Y], [X, Z], [Y, Z]]

--        /\
--   ____/__\
--  /\  /\  /
-- /__\/*_\/
shape11 = [[], [X], [Y], [Y, X], [X, Z]]

shapes :: [Shape]
shapes =
  [ shape11
  , shape10
  , shape9
  , shape8
  , shape7
  , shape6
  , shape5
  , shape4
  , shape3
  , shape2
  , shape1
  ]

rotate :: Direction -> Direction
rotate X = Y
rotate Y = Z
rotate Z = X

rotateShape :: Shape -> Shape
rotateShape = map (map rotate)

rotations :: Shape -> [Shape]
rotations = take 3 . iterate rotateShape

reflect :: Direction -> Direction -> Direction
reflect X X = X
reflect X Y = Z
reflect X Z = Y
reflect Y X = Z
reflect Y Y = Y
reflect Y Z = X
reflect Z X = Y
reflect Z Y = X
reflect Z Z = Z

reflectShape :: Direction -> Shape -> Shape
reflectShape = map . map . reflect

reflections :: Shape -> [Shape]
reflections shape = [reflectShape d shape | d <- [X, Y, Z]]

walk :: Path -> Int -> Maybe Int
walk [] n = Just n
walk (d:ds) n = walk ds =<< neighbour d n

tryPlaceShape :: Set Int -> Int -> Shape -> Maybe (Set Int)
tryPlaceShape grid n shape =
  case Set.fromList <$> mapM (\path -> walk path n) shape of
    Just ns | ns `Set.isSubsetOf` grid -> Just ns
    _ -> Nothing

placeShape :: Shape -> Set Int -> Set (Set Int)
placeShape shape grid =
  Set.fromList $ catMaybes
    [ tryPlaceShape grid n s
    | n <- Set.toList grid
    , s <- rotations shape ++ reflections shape
    ]

placeShapes :: [Shape] -> Set Int -> [([Set Int], Set Int)]
placeShapes [] grid = [([], grid)]
placeShapes (shape:shapes) grid =
  [ (s:ss, grid')
  | s <- Set.toList (placeShape shape grid)
  , (ss, grid') <- placeShapes shapes (grid \\ s)
  ]

fullGrid :: Set Int
fullGrid = Set.fromList [1..48]

solve :: Set Int -> Maybe [Set Int]
solve blockers = listToMaybe $ map fst $ placeShapes shapes grid
  where
    grid = fullGrid \\ blockers

main :: IO ()
main = do
  args <- getArgs
  let blockers = Set.fromList $ map read args
  if Set.size blockers == 7 && blockers `Set.isSubsetOf` fullGrid
    then case solve blockers of
           Nothing ->
             putStrLn "No solution."
           Just shapes ->
             putStrLn $ unlines $
                 "Solution:"
               : map (unwords . map show . Set.toList) shapes
    else putStrLn "Invalid input."
