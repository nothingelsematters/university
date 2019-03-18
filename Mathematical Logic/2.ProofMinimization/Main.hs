module Main where

import System.IO

import Proof
import ProofMinimizator


main :: IO ()
main = do task <- getLine
          mainloop (fromTask . getTask $ task)

mainloop :: HalfProof -> IO ()
mainloop proof = do
    ineof <- isEOF
    if ineof
    then if failure proof
        then putStrLn incorrectString
        else case minimizeProof . sortProof $ proof of
            Nothing -> putStrLn incorrectString
            Just p -> showProof p
    else do ex <- getLine
            case addExpression (getExpression ex) proof of
                Nothing -> putStrLn incorrectString
                Just newproof -> mainloop newproof

incorrectString = "Proof is incorrect"

showProof :: SortedProof -> IO ()
showProof sp = do
    putStrLn . show $ (task sp)
    listShower showExpression (length . expressions $ sp) (expressions sp)

listShower :: (a -> Int -> String) -> Int -> [a] -> IO ()
listShower _ _ [] = do
    putStr ""
listShower f n (h:t) = do
    listShower f (n - 1) t
    putStrLn $ f h n
