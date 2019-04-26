module Main where

import Logic.Parsing.ExpressionParser
import Logic.CompleteProof

main :: IO ()
main = do
    expr <- getLine
    case completeProof . getExpression $ expr of
        Nothing   -> putStrLn ":("
        Just smth -> print    smth
