module SpecBlock5
  ( fifthBlockTest
  ) where

import Data.Either (isLeft)

import Block5
import Generator (listGenerator)
import InstanceTest (simpleTests)

import Hedgehog (Property, diff, forAll, property)
import qualified Hedgehog.Gen as Gen (double, int)
import qualified Hedgehog.Range as Range (constant)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)


expressionTests :: TestTree
expressionTests = simpleTests "expression simple tests"
  [ ("summary test", eval (Constant 10 :+: Constant 23) == Right (10 + 23))
  , ( "difference test"
    , eval (Constant 10 :-: Constant 23) == Right (10 - 23)
    )
  , ( "multiplication test"
    , eval (Constant 10 :*: Constant 23) == Right (10 * 23)
    )
  , ( "division test"
    , eval (Constant 10 :/: Constant 23) == Right (10 `div` 23)
    )
  , ( "power test"
    , eval (Constant 10 :^: Constant 23) == Right (10 ^ (23 :: Int))
    )
  , ( "summary test"
    , eval (Constant 10 :+: Constant 23 :+: Constant 12) ==
        Right (10 + 23 + 12)
    )
  , ( "difference test"
    , eval (Constant 10 :-: Constant 23 :-: Constant 12) == Right (10 - 23 - 12)
    )
  , ( "multiplication test"
    , eval (Constant 10 :*: Constant 23 :*: Constant 12) == Right (10 * 23 * 12)
    )
  , ( "division test"
    , eval (Constant 10 :/: Constant 23 :/: Constant 12) ==
        Right (10 `div` 23 `div` 12)
    )
  , ( "power test"
    , eval (Constant 10 :^: Constant 3 :^: Constant 2) ==
        Right ((10 :: Int) ^ (3 :: Int) ^ (2 :: Int))
    )
  , ( "plus, minus, multiplication, division"
    , eval (Constant 10 :+: Constant 2 :*: Constant 30 :-: Constant 2 :/:
        Constant 2) == Right (10 + 2 * 30 - 2 `div` 2)
    )
  , ( "different operations"
    , eval (Constant 10 :+: Constant 2 :*: Constant 3 :^: Constant 2 :/:
        Constant 3 :-: Constant 10) ==
          Right (10 + 2 * 3 ^ (2 :: Int) `div` 3 - 10)
    )
  , ( "division by zero error"
    , isLeft . eval $ Constant 18 :/: (Constant 2 :-: Constant 2)
    )
  , ( "negative power error"
    , isLeft . eval $ Constant 200 :^: (Constant 2 :-: Constant 10)
    )
  ]

movingTests :: TestTree
movingTests = simpleTests "sample tests"
  [ ( "first sample test"
    , moving 4 ([1, 5, 3, 8, 7, 9, 6] :: [Double]) ==
      [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
    )
  , ( "second sample test"
    , moving 2 ([1, 5, 3, 8, 7, 9, 6] :: [Double]) ==
      [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
    )
  ]

movingRandomTests :: Property
movingRandomTests = property $ do
  list <- forAll . listGenerator . Gen.double $ Range.constant (-10) 10
  frame <- forAll . Gen.int $ Range.constant 2 10
  diff (moving frame list) epsCmp $ map (naive frame list) [1..length list]
    where
      naive :: Int -> [Double] -> Int -> Double
      naive frame list index =
        (/ realToFrac (if index - frame > 0 then frame else index)) . sum .
        drop (index - frame) . take index $ list

      epsCmp :: [Double] -> [Double] -> Bool
      epsCmp x y = and . map (uncurry $ \a b -> abs (a - b) <= 1e-7) $ zip x y


fifthBlockTest :: TestTree
fifthBlockTest =  testGroup "Fifth block"
    [ expressionTests
    , movingTests
    , testProperty "moving random tests" movingRandomTests
    ]
