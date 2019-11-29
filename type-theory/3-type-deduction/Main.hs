module Main where

import Control.Monad (join)
import Lambda.Parse.ExpressionParser (getExpression)
import Lambda.TypeDeduction (deduceType)

main :: IO ()
main = join $ putStr . maybe "Expression has no type\n" show . deduceType . getExpression <$> getContents
