module Main (main) where

import Lib
import Conf

main :: IO ()
main = 
    let gwc = gridWithCoords grid
    in outputGrid gwc
