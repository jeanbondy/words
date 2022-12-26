module Lib
    ( outputGrid
    , formatGrid
    , someString
    , findWord
    , findWords
    , findWordInLine
    , skew
    , coordsGrid
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)
import Conf

data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)

type Grid a = [[a]]
-- helps the human reader of the sourcecode to understand
-- what's going on

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell    
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)


formatGrid :: Grid Cell -> String
formatGrid grid = unlines ( (map . map) cell2char grid )
-- I don't understand why the proposed solution doesn't work:
-- Couldn't match type: [Char]
 --                    with: Grid Cell -> String
--      Expected: Grid Cell -> String
--        Actual: String
--    • Possible cause: ‘unlines’ is applied to too many arguments
--      In the expression: unlines chargrid
--formatGrid =
--    let charGrid = (map . map) cell2char grid
--    in unlines charGrid

cell2char :: Cell -> Char
cell2char (Cell _ c) = c

skew :: Grid Char -> Grid Char
skew [] = []
--skew (x:xs) = x : skew (map (prepend "_") xs)
--    where prepend a b = a ++ b
skew (l:ls) = l : skew (map indent ls)
    where indent line = "_" ++ line
    
diagonalize :: Grid Char -> Grid Char
diagonalize = transpose . skew

findWord :: Grid Char -> String -> Maybe String
-- findWord grid word = or $ map (findWordInLine word) grid
findWord grid word =
    let horizontal = grid
        vertical = transpose grid
--        diagonal1 = transpose (skew horizontal)
--        diagonal2 = transpose (skew (map reverse horizontal))
        diagonal1 = diagonalize horizontal
        diagonal2 = diagonalize (map reverse horizontal)
        total = horizontal ++ vertical ++ diagonal1 ++ diagonal2
        reversed = map reverse (total)
        grids =total ++ reversed 
        found = or $ map (findWordInLine word) grids
    in if found then Just word else Nothing
    
findWords :: Grid Char -> [String] -> [String]    
findWords grid words = 
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf
-- we could use isInfixOf, but creating the alias
-- documents our intent better.
-- findWordInLine word line = word `isInfixOf` line

someString :: String
someString = "someString"


