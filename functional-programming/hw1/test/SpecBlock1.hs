{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SpecBlock1
  ( firstBlockTest
  ) where

import Data.Foldable (toList)
import Data.Function (on)
import Data.List (delete, sort)

import Block1
import Generator (intGenerator, randomList)
import InstanceTest (simpleTests)

import Hedgehog (Property, PropertyT, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen (choice, element, enum, integral, list)
import qualified Hedgehog.Range as Range (constant)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)


weekdayOrder :: [Weekday]
weekdayOrder = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

forWeekdays :: (Weekday -> Bool) -> Bool
forWeekdays = flip all weekdayOrder

nextDayCorrectnessTest :: Bool
nextDayCorrectnessTest = all checkNext [0 .. days - 1]
  where
    days :: Int
    days = length weekdayOrder

    checkNext :: Int -> Bool
    checkNext index = nextDay (weekdayOrder !! index) ==
      weekdayOrder !! ((index + 1) `mod` days)

afterDaysNext :: Bool
afterDaysNext = forWeekdays $ \wd -> nextDay wd == afterDays wd 1

isWeekendTest :: Bool
isWeekendTest = forWeekdays $ \wd -> isWeekend wd == isWeekend' wd
  where
    isWeekend' :: Weekday -> Bool
    isWeekend' Saturday = True
    isWeekend' Sunday   = True
    isWeekend' _        = False

daysToPartyTest :: Bool
daysToPartyTest = forWeekdays test
  where
    daysToParty' :: Weekday -> Int
    daysToParty' wd =
      length . takeWhile (/= Friday) . dropWhile (/= wd) $ cycle weekdayOrder

    test :: Weekday -> Bool
    test wd = daysToParty wd == daysToParty' wd

afterDaysCorrectnessTest :: Property
afterDaysCorrectnessTest = property $ do
  x <- forAll . Gen.integral $ Range.constant 0 1000
  wd <- forAll $ Gen.enum Monday Sunday
  afterDays wd x === (cycle weekdayOrder !! (x + fromEnum wd))

weekdayTests :: TestTree
weekdayTests = simpleTests "weekday tests"
  [ ("next day correctness test", nextDayCorrectnessTest)
  , ("after days next correctness test", afterDaysNext)
  , ("is weekend function test", isWeekendTest)
  , ("daysToParty function tests", daysToPartyTest)
  ]

weekdayChecks :: TestTree
weekdayChecks = testGroup "weekday rules" $
  [testProperty "random afterDays correctness test" afterDaysCorrectnessTest]

smallRange :: Monad m => PropertyT m Int
smallRange = forAll . Gen.integral $ Range.constant 1 30

smallNat :: Monad m => PropertyT m Nat
smallNat = toEnum <$> smallRange

backAndForthTest :: Property
backAndForthTest = property $ smallRange >>= \x ->
  fromEnum ((toEnum :: Int -> Nat) x) === x

intAnalogueTest :: (Nat -> Nat -> Nat) -> (Int -> Int -> Int) -> Property
intAnalogueTest natOp intOp = property $ do
  x <- smallRange
  y <- smallRange
  fromEnum (on natOp toEnum x y) === x `intOp` y

intSameTest
  :: (Eq a, Show a)
  => (Nat -> Nat -> a)
  -> (Int -> Int -> a)
  -> Property
intSameTest natOp intOp = property $ do
  x <- smallRange
  y <- smallRange
  on natOp toEnum x y === x `intOp` y

commutativityTest :: (Nat -> Nat -> Nat) -> Property
commutativityTest op = property $ do
  x <- smallNat
  y <- smallNat
  x `op` y === y `op` x

associativityTest :: (Nat -> Nat -> Nat) -> Property
associativityTest op = property $ do
  x <- smallNat
  y <- smallNat
  z <- smallNat
  x `op` (y `op` z) === (x `op` y) `op` z

revertingTest :: (Nat -> Nat -> Nat) -> (Nat -> Nat -> Nat) -> Property
revertingTest to from = property $ do
  x <- smallNat
  y <- smallNat
  x === (x `to` y) `from` y

naturalRules :: TestTree
naturalRules = testGroup "Natural number tests" . map (uncurry testProperty) $
  [ ("fromEnum and toEnum test", backAndForthTest)
  , ("plus commutativity", commutativityTest (^+))
  , ("plus associativity", associativityTest (^+))
  , ("plus works like Int plus", intAnalogueTest (^+) (+))
  , ("minus reverting plus", revertingTest (^+) (^-))
  , ("multiplication commutativity", commutativityTest (^*))
  , ("multiplication associativity", associativityTest (^*))
  , ("multiply works like Int multiply", intAnalogueTest (^*) (*))
  , ("division works like Int division", intAnalogueTest (^/) div)
  , ("division reverting multiplicaton", revertingTest (^*) (^/))
  , ("mod works like Int mod", intAnalogueTest natMod mod)
  , ("less works like Int less", intSameTest (<) (<))
  , ("less or greater works like Int less or greater", intSameTest (<=) (<=))
  , ("equals works like Int equals", intSameTest (==) (==))
  , ("not equals works like Int not equals", intSameTest (/=) (/=))
  , ("greater works like Int greater", intSameTest (>) (>))
  , ("greater or equals works like Int greater or equals", intSameTest (>=) (>=))
  ]


randomTree :: Monad m => PropertyT m (Tree Int)
randomTree = (fromList <$>) . forAll $ Gen.list (Range.constant 1 100) intGenerator

randomTreeTest ::(Tree Int -> PropertyT IO ()) -> Property
randomTreeTest rule = property $ randomTree >>= rule

nonEmptyTest :: Property
nonEmptyTest = randomTreeTest $ \tree -> assert . not . isEmpty $ tree

sizeTest :: Property
sizeTest = property $ do
  list <- randomList
  length list === (size . fromList $ list)

data Action a
  = Add a
  | Remove a
  deriving Show

performAction :: (Eq a, Ord a) => ([a], Tree a) -> Action a -> ([a], Tree a)
performAction (list, tree) (Add    el) = (el : list, insert tree el)
performAction (list, tree) (Remove el) = (delete el list, remove tree el)

treeOperationIteration
  :: PropertyT IO ([Int], Tree Int)
  -> PropertyT IO ([Int], Tree Int)
treeOperationIteration prop = do
  (list, tree) <- prop
  let addGen = Add <$> intGenerator
  let removeGen = Remove <$> Gen.element list
  performAction (list, tree) <$> forAll (Gen.choice [addGen, removeGen])

treeOperationTest :: Property
treeOperationTest = property $ do
  list <- forAll . Gen.list (Range.constant 500 700) $ intGenerator
  (list', tree') <- (!! 500) . iterate treeOperationIteration $
    pure (list, fromList list)
  sort list' === toList tree'

treeRules :: TestTree
treeRules = testGroup "Tree rule tests" . map (uncurry testProperty) $
  [ ("non empty test", nonEmptyTest)
  , ("size test", sizeTest)
  , ("common operations test", treeOperationTest)
  ]

firstBlockTest :: TestTree
firstBlockTest = testGroup "First block"
    [ weekdayTests
    , weekdayChecks
    , naturalRules
    , treeRules
    ]
