module Main where

import System.IO     (isEOF)
import Control.Monad (join)

import Logic.Parsing.ExpressionParser
import Logic.Proof
import Logic.CheckProof

main :: IO ()
main = join $ mainloop <$> fromTask . getTask <$> getLine
    where
        mainloop proof = do
            ineof <- isEOF
            if ineof
                then putStrLn $
                    if obtained proof
                        then "Proof is correct"
                        else "Required hasn't been proven"
                else join $ either putStrLn mainloop <$> processLine proof <$> getExpression <$> getLine
