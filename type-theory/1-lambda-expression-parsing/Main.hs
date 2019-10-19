module Main where

import Control.Monad (join)
import Lambda.Parse.ExpressionParser

main :: IO ()
main = join $ putStrLn <$> show <$> getExpression <$> getContents
