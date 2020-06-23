{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Task7
Description : Seventh task of zero home work
Copyright   : (c) Simon Naumov, 2020
License     : MIT
Maintainer  : daretoodefy@gmail.com
Stability   : experimental

This module defines functions from seventh homework task. It consists of
typed expressions and each of their subexpressions typed.
-}
module Task7
  ( firstExpression
  , secondExpression
  , thirdExpression
  ) where

import Data.Either (lefts, rights)

-- | This function was designed in order to study how to type the expressions
-- and subexpressions, it wasn't supposed for practical use.
firstExpression :: Bool
firstExpression = nullHeadApply mappedList
  where
    dorian :: String
    dorian = "Dorian "

    concatenation :: String -> String -> String
    concatenation = (++)

    concatDorian :: String -> String
    concatDorian = concatenation dorian

    grey :: String
    grey = " Grey"

    makePair :: (String -> String) -> String -> ((String -> String), String)
    makePair = (,)

    pairWithConcatDorian :: String -> ((String -> String), String)
    pairWithConcatDorian = makePair concatDorian

    dorianGrey :: (String -> String, String)
    dorianGrey = pairWithConcatDorian grey

    dorianGreyList :: [(String -> String, String)]
    dorianGreyList = [dorianGrey]

    uncurry'
      :: ((String -> String) -> String -> String)
      -> ((String -> String), String)
      -> String
    uncurry' = uncurry

    id' :: (String -> String) -> String -> String
    id' = id

    map'
      :: ((String -> String, String) -> String)
      -> [(String -> String, String)]
      -> [String]
    map' = map

    uncurryId :: (String -> String, String) -> String
    uncurryId = uncurry' id'

    mapper :: [(String -> String, String)] -> [String]
    mapper = map' uncurryId

    mappedList :: [String]
    mappedList = mapper dorianGreyList

    head' :: [String] -> String
    head' = head

    null' :: String -> Bool
    null' = null

    composition :: (String -> Bool) -> ([String] -> String) -> [String] -> Bool
    composition = (.)

    nullCompose :: ([String] -> String) -> [String] -> Bool
    nullCompose = composition null'

    nullHead :: [String] -> Bool
    nullHead = nullCompose head'

    applyOperator :: ([String] -> Bool) -> [String] -> Bool
    applyOperator = ($)

    nullHeadApply :: [String] -> Bool
    nullHeadApply = applyOperator nullHead

-- | This function was designed in order to study how to type the expressions
-- and subexpressions, it wasn't supposed for practical use.
secondExpression :: (Num a, Num b) => [(a, b)]
secondExpression = zipper eitherList
  where
    one :: Num a => a
    one = 1

    two :: Num a => a
    two = 2

    six :: Integral a => a
    six = 6

    plus :: Num a => a -> a -> a
    plus = (+)

    power :: Num a => a -> Integer -> a
    power = (^)

    plusOne :: Num a => a -> a
    plusOne = plus one

    three :: Num a => a
    three = plusOne two

    powerTwo :: Num a => Integer -> a
    powerTwo = power two

    sixtyFour :: Num a => a
    sixtyFour = powerTwo six

    left :: a -> Either a b
    left = Left

    right :: b -> Either a b
    right = Right

    leftThree :: Num a => Either a b
    leftThree = left three

    rightSixtyFour :: Num b => Either a b
    rightSixtyFour = right sixtyFour

    eitherList :: (Num a, Num b) => [Either a b]
    eitherList = [leftThree, rightSixtyFour]

    rights' :: [Either a b] -> [b]
    rights' = rights

    lefts' :: [Either a b] -> [a]
    lefts' = lefts

    zip' :: [a] -> [b] -> [(a, b)]
    zip' = zip

    zipper :: forall a b . [Either a b] -> [(a, b)]
    zipper x = zip' (lefts' x :: [a]) (rights' x :: [b])

-- | This function was designed in order to study how to type the expressions
-- and subexpressions, it wasn't supposed for practical use.
thirdExpression :: Integral a => a -> Bool
thirdExpression = \x -> (isMod4 x) `impl` (isMod2 x)
  where
    not' :: Bool -> Bool
    not' = not

    orOperator :: Bool -> Bool -> Bool
    orOperator = (||)

    impl :: Bool -> Bool -> Bool
    impl x y = orNotX y
      where
        notX :: Bool
        notX = not' x

        orNotX :: Bool -> Bool
        orNotX = orOperator notX

    mod' :: Integral a => a -> a -> a
    mod' = mod

    two :: Integral a => a
    two = 2

    zero :: Integral a => a
    zero = 0

    four :: Integral a => a
    four = 4

    equals :: Integral a => a -> a -> Bool
    equals = (==)

    isMod2 :: forall a . Integral a => a -> Bool
    isMod2 x = equalsXModTwo zero
      where
        xMod :: Integral a => a -> a
        xMod = mod' x

        xModTwo :: Integral a => a
        xModTwo = xMod two

        equalsXModTwo :: Integral a => a -> Bool
        equalsXModTwo = equals xModTwo

    isMod4 :: forall a . Integral a => a -> Bool
    isMod4 x = equalsXModFour zero
      where
        xMod :: Integral a => a -> a
        xMod = mod' x

        xModFour :: Integral a => a
        xModFour = xMod four

        equalsXModFour :: Integral a => a -> Bool
        equalsXModFour = equals xModFour
