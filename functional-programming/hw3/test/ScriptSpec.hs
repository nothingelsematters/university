{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScriptSpec
  ( scriptTests
  ) where

import Hedgehog (Property, diff, forAll, property, (===))
import qualified Hedgehog.Gen as Gen (int)
import qualified Hedgehog.Range as Range (constant)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Script (Script (..), eval)


log2 :: Script a => a (Int -> a Int)
log2 = function1 \a ->
  var (int (-1)) \result ->
  var (int 1)    \accum  ->
  sWhile (a @>= deref accum)
    ( accum  @= deref accum  @+ deref accum
    # result @= deref result @+ raw  1
    )
    #
  deref result

log2' :: Int -> Int
log2' = floor . logBase 2 . (realToFrac :: Int -> Double)

sample :: Script a => a String
sample =
  var (int 10) \a ->
  var (string "sample program with 3 types: log2 10 ") \result ->
  sIf ((log2 @$$ deref a) @> raw 2)
    (result @= deref result @++ raw "is more than 2")
    (result @= deref result @++ raw "is less than 2 or equals 2")
    #
  deref result

log2Test :: Property
log2Test = property do
  x <- forAll . Gen.int $ Range.constant 1 4097
  log2' x === eval (log2 @$$ raw x)

sampleTest :: Property
sampleTest = property $ diff (eval sample) (==)
  "sample program with 3 types: log2 10 is more than 2"

scriptTests :: TestTree
scriptTests = testGroup "script interpreter" $ map (uncurry testProperty)
  [ ("log2 simple test",                     log2Test)
  , ("log2 sample program with three types", sampleTest)
  ]

