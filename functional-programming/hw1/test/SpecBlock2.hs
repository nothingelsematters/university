{-# LANGUAGE OverloadedStrings #-}

module SpecBlock2
  ( secondBlockTest
  ) where

import Data.Foldable (toList)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.Split as List (splitOn)

import Block1
import Block2
import Generator (intGenerator, listGenerator, randomChar, randomList,
                  randomString, stringGenerator)
import InstanceTest (foldableTest, simpleTests, testProperties)

import Hedgehog (Property, PropertyT, forAll, property, (===))
import qualified Hedgehog.Gen as Gen (nonEmpty)
import qualified Hedgehog.Range as Range (constant)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)


intTree :: Monad m => PropertyT m (Tree Int)
intTree = fromList <$> randomList

listTree :: Monad m => PropertyT m (Tree [Int])
listTree = fromList <$> (forAll . listGenerator . listGenerator $ intGenerator)

sortTest :: Property
sortTest = property $ randomList >>= \xs -> toList (fromList xs) === sort xs

foldableTreeTests :: TestTree
foldableTreeTests = testProperties "Foldable Tree tests" $
  ("rule \"toList . fromList ≡ sort\"", sortTest) :
  foldableTest intTree listTree

splitterSimpleTests :: TestTree
splitterSimpleTests = simpleTests "splitOn and joinWith function tests" $
  [ ( "task description test"
    , splitOn '/' ("path/to/file" :: String) == ("path" :| ["to", "file"])
    )
  , ( "task description test"
    , joinWith '/' ("path" :| ["to", "file"]) == "path/to/file"
    )
  ]

joinWithTests :: TestTree
joinWithTests = testProperty "rule \"joinWith x . splitOn x ≡ id\"" .
  property $ do
    string <- randomString
    char <- randomChar
    (joinWith char . splitOn char $ string) === string

joinWithAnalogueTest :: TestTree
joinWithAnalogueTest = testProperty "intercalate analogue test" . property $ do
  randomStrings <- forAll $ Gen.nonEmpty (Range.constant 0 10) stringGenerator
  char <- randomChar
  joinWith char randomStrings ===
    intercalate [char] (toList randomStrings)

spliOnAnalogueTest :: TestTree
spliOnAnalogueTest = testProperty "splitOn analogue test" . property $ do
  string <- randomString
  char <- randomChar
  toList (splitOn char string) === List.splitOn [char] string

secondBlockTest :: TestTree
secondBlockTest = testGroup "Second block"
    [ foldableTreeTests
    , splitterSimpleTests
    , joinWithTests
    , joinWithAnalogueTest
    , spliOnAnalogueTest
    ]
