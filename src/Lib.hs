module Lib
    ( outputGrid
    , formatGrid
    , someString
    , grid
    , languages
    , findWord
    , findWords
    , findWordInLine
    , skew
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]
-- helps the human reader of the sourcecode to understand
-- what's going on

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

skew :: Grid -> Grid
skew [] = []
--skew (x:xs) = x : skew (map (prepend "_") xs)
--    where prepend a b = a ++ b
skew (l:ls) = l : skew (map indent ls)
    where indent line = "_" ++ line
    
diagonalize :: Grid -> Grid
diagonalize = transpose . skew

findWord :: Grid -> String -> Maybe String
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
    
findWords :: Grid -> [String] -> [String]    
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

grid :: Grid
grid = [  "__C________R___"
        , "__SI________U__"
        , "__HASKELL____B_"
        , "__A__A_____S__Y"
        , "__R___B___C____"
        , "__PHP____H_____"
        , "____S_LREP_____"
        , "____I__M_Y__L__"
        , "____L_E__T_O___"
        , "_________HB____"
        , "_________O_____"
        , "________CN_____"
        ]

languages :: [String]
languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]
