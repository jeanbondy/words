module Lib
    ( outputGrid
    , formatGrid
    , someString
    , grid
    , languages
    , findWord
    , findWordInLine
    ) where

import Data.List (isInfixOf)

type Grid = [String]
-- helps the human reader of the sourcecode to understand
-- what's going on

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

findWord :: Grid -> String -> Bool
-- findWord grid word = or $ map (findWordInLine word) grid
findWord grid word =
    let grids = grid ++ (map reverse grid)
    in or $ map (findWordInLine word) grids

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
