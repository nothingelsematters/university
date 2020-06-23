{-|
Module      : Main
Description : First practice
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental

This module consists of sorting functions, input and output interface to them.
-}
module Main
  ( main
  , solve
  ) where

import Data.Function (on)

-- | Read a string consisting of Integers, sort the given input and output the
-- results. Sort function is defined by the first Integer given:
--
--     * 0 stands for quick sort
--
--     * 1 stands for merge sort
--
--     * 2 stands for bubble sort
main :: IO ()
main = interact solve

readInt :: String -> Int
readInt = read

showInt :: Int -> String
showInt = show

-- | Sorts the input and output the results. Sort function is defined by the
-- first Integer given:
--
--     * 0 stands for quick sort
--
--     * 1 stands for merge sort
--
--     * 2 stands for bubble sort
solve :: String -> String
solve = either id (unwords . map showInt) . sortSpecified . map readInt . words
  where
    sortSpecified :: [Int] -> Either String [Int]
    sortSpecified (x:xs) = ($ xs) <$> choose x
    sortSpecified _        = errorMsg

    choose :: Ord a => Int -> Either String ([a] -> [a])
    choose 0 = Right quickSort
    choose 1 = Right mergeSort
    choose 2 = Right bubbleSort
    choose _ = errorMsg

    errorMsg = Left "undefined sort chosen"

quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = commonPart (>=) ++ x:commonPart (<)
  where
    commonPart op = quickSort $ filter (op x) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort xs
  | length xs > 1 = on merge (\f -> mergeSort $ f size xs) take drop
  | otherwise     = xs
    where
      size = (length xs) `div` 2

      merge :: Ord a => [a] -> [a] -> [a]
      merge [] ys = ys
      merge ys [] = ys
      merge (y:ys) (z:zs)
          | y < z     = y:merge ys (z:zs)
          | otherwise = z:merge (y:ys) zs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = iterate rise xs !! length xs
  where
    rise :: Ord a => [a] -> [a]
    rise (x:y:ys)
        | x > y     = y:rise (x:ys)
        | otherwise = x:rise (y:ys)
    rise x = x
