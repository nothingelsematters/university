module Main where

import System.IO

import Parsing.ProofParser
import Expression
import IntuitionisticConversion

main :: IO ()
main = do task <- getLine
          putStrLn . show $ Task (hypotheses . getTask $ task) 
                           (Not (Not (result . getTask $ task)))
          mainloop $ newLineHandler (hypotheses . getTask $ task)

mainloop :: LineHandler -> IO ()
mainloop lh = do
    ineof <- isEOF
    if not ineof
        then do ex <- getLine
                case doubleNegate lh $ getExpression ex of
                    (newlh, exprslist) -> 
                        do mapM_ (putStrLn . show) exprslist
                           mainloop newlh
        else return ()
