module Main where

import Control.Monad (join)
import Lambda.Parse.ExpressionParser
import Lambda.Expression


main :: IO ()
main = do
    ints <- map (read::(String -> Integer)) <$> words <$> getLine
    join $ mainloop (ints !! 0) (ints !! 1) 0 <$> makeUnique <$> getExpression <$> getLine

mainloop :: Integer -> Integer -> Integer -> Expression -> IO ()
mainloop step repetitions was expr = do
    if (was > repetitions)
        then return ()
        else do
            if (was `mod` step == 0 || was == repetitions)
                then print expr
                else return ()
            mainloop step repetitions (was + 1) $ reduce expr
