{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpecBlock4
  ( fourthBlockTest
  ) where

import qualified Data.List.NonEmpty as NonEmpty (NonEmpty ((:|)))

import Block4
import Generator (intGenerator, intListGenerator)
import InstanceTest (applicativeTest, foldableTest, functorTest, simpleTests,
                     testProperties, traversableTest)

import Hedgehog (Gen, PropertyT, forAll)
import qualified Hedgehog.Gen as Gen (constant, frequency, list, nonEmpty)
import qualified Hedgehog.Range as Range (constant)
import Test.Tasty (TestTree, testGroup)


stringSumTest :: TestTree
stringSumTest = simpleTests "stringSum function tests"
  [ ("int parse test", stringSum "5" == Just 5)
  , ("simple sum test", stringSum "5 8" == Just 13)
  , ("simple error test", stringSum "!4" == Nothing)
  , ("random test", stringSum "1  2     8  19" == Just 30)
  , ("empty test", stringSum "" == Just 0)
  , ("negative numbers test", stringSum "-100 101" == Just 1)
  ]

nonEmptyGen :: Gen a -> Gen (NonEmpty a)
nonEmptyGen gen = (\(a NonEmpty.:| b) -> a :| b) <$>
  Gen.nonEmpty (Range.constant 0 5) gen

nonEmptyMaker :: PropertyT IO (NonEmpty Int)
nonEmptyMaker = forAll . nonEmptyGen $ intGenerator

fromList :: forall a . ([Bool], [a]) -> Tree a
fromList p = snd $ fromList' p
  where
    fromList' :: ([Bool], [a]) -> ([a], Tree a)
    fromList' ((b:bs), (x:xs)) =
      if b
      then (xs, Leaf x)
      else (rest', Branch left right)
        where
          (rest, left) = fromList' (bs, (x:xs))
          (rest', right) = fromList' (bs, rest)
    fromList' _ = undefined

treeGen :: Gen a -> Gen (Tree a)
treeGen gen = fromList <$> ((,) <$> bools <*> elementList)
    where
      bools = Gen.list (Range.constant 200 400) $
        Gen.frequency [(4, Gen.constant True), (1, Gen.constant False)]
      elementList = Gen.list (Range.constant 200 400) gen

treeMaker :: PropertyT IO (Tree Int)
treeMaker = forAll . treeGen $ intGenerator

fourthBlockTest :: TestTree
fourthBlockTest = testGroup "Fourth block"
    [ stringSumTest
    , testProperties "Functor NonEmpty test" . functorTest $ nonEmptyMaker
    , testProperties "Applicative NonEmpty test" . applicativeTest $ nonEmptyMaker
    , testProperties "Foldable NonEmpty test" . foldableTest nonEmptyMaker .
        forAll . nonEmptyGen $ intListGenerator
    , testProperties "Traversable NonEmpty test" $
        traversableTest nonEmptyGen nonEmptyGen

    , testProperties "Functor Tree test" . functorTest $ treeMaker
    , testProperties "Applicative Tree test" . applicativeTest $ treeMaker
    , testProperties "Foldable Tree test" . foldableTest treeMaker .
        forAll . treeGen $ intListGenerator
    , testProperties "Traversable Tree test" $
        traversableTest treeGen treeGen
    ]
