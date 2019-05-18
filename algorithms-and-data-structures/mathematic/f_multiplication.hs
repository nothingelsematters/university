module Main where

import Data.Function (on)

main :: IO ()
main = do
    left <- getLine
    right <- getLine
    putStrLn . show $ on (*) toIntg left right
        where toIntg x = read x::Integer
