module Main where

import Grammar
import Tokens

main :: IO ()
main = do
    expression <- getLine
    putStrLn . show . parseExpression . alexScanTokens $ expression
