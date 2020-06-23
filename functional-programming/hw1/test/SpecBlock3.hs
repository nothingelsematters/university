module SpecBlock3
  ( thirdBlockTest
  ) where

import Block3
import Generator (intGenerator, intListGenerator, stringGenerator)
import InstanceTest (applyMaker, monoidTest, semigroupTest, testProperties)

import Data.Either (fromLeft, fromRight, isLeft, isRight)
import qualified Data.List.NonEmpty as NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust, isJust)

import Hedgehog (Gen, Property, PropertyT, forAll, property, (===))
import qualified Hedgehog.Gen as Gen (choice, constant, int, list, maybe,
                                      nonEmpty)
import qualified Hedgehog.Range as Range (constant)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)


listMaybeList :: PropertyT IO ([Maybe [Int]])
listMaybeList = forAll . Gen.list (Range.constant 0 100) . Gen.maybe $
  intListGenerator

maybeConcatTest :: TestTree
maybeConcatTest = testProperty "maybe concat correctness test" . property $ do
  argument <- listMaybeList
  maybeConcat argument === (concat . map fromJust . filter isJust $ argument)

listEitherList :: PropertyT IO ([Either [Int] [Int]])
listEitherList = forAll . Gen.list (Range.constant 0 100) . Gen.choice $
  [Left <$> intListGenerator, Right <$> intListGenerator]

eitherConcatTest :: TestTree
eitherConcatTest = testProperty "either concat correctness test" . property $ do
  argument <- listEitherList
  let concatter m f = concat . map (m undefined) . filter f $ argument
  eitherConcat argument ===
    (concatter fromLeft isLeft, concatter fromRight isRight)


nonEmptyMaker :: PropertyT IO (NonEmpty Int)
nonEmptyMaker = forAll $ (\(a NonEmpty.:| b) -> a :| b) <$>
  (Gen.nonEmpty range . Gen.int $ range)
    where
      range = Range.constant 0 100

nameGenerator :: Gen Name
nameGenerator = Name <$> stringGenerator

thisOrThatMaker :: PropertyT IO (ThisOrThat Int Int)
thisOrThatMaker = do
  thisInt <- forAll intGenerator
  thatInt <- forAll intGenerator
  forAll . Gen.choice . map Gen.constant $
    [This thisInt, That thatInt, Both thisInt thatInt]

associativityTest :: PropertyT IO Int -> Property
associativityTest maker = property $ do
  let endoMaker = Endo . (*) <$> maker
  x <- endoMaker
  y <- endoMaker
  z <- endoMaker
  arg <- maker
  getEndo (x <> (y <> z)) arg === getEndo ((x <> y) <> z) arg

endoSemigroupTest :: PropertyT IO Int -> [(TestName, Property)]
endoSemigroupTest maker =
  [("rule \"x <> (y <> z) = (x <> y) <> z\"", associativityTest maker)]

rightMemptyTest :: PropertyT IO Int-> Property
rightMemptyTest maker = property $ do
  let endoMaker = Endo . (*) <$> maker
  x <- endoMaker
  arg <- maker
  getEndo (x <> mempty) arg === getEndo x arg

leftMemptyTest :: PropertyT IO Int-> Property
leftMemptyTest maker = property $ do
  let endoMaker = Endo . (*) <$> maker
  x <- endoMaker
  arg <- maker
  getEndo (mempty <> x) arg === getEndo x arg

mconcatTest :: Gen Int -> Property
mconcatTest generator = property $ do
  arg <- forAll generator
  list <- (map (Endo . (*)) <$>) . forAll .
    Gen.list (Range.constant 0 100) $ generator
  getEndo (mconcat list) arg === getEndo (foldr (<>) mempty list) arg

endoMonoidTest :: Gen Int -> [(TestName, Property)]
endoMonoidTest generator = endoSemigroupTest maker ++
  ("rule \"mconcat = foldr (<>) mempty\"", mconcatTest generator) :
  applyMaker maker
  [ ("rule \"x <> mempty = x\"", rightMemptyTest)
  , ("rule \"mempty <> x = x\"", leftMemptyTest)
  ]
    where
      maker = forAll generator

thirdBlockTest :: TestTree
thirdBlockTest = testGroup "Third block"
    [ maybeConcatTest
    , eitherConcatTest
    , testProperties "NonEmpty Semigroup test" . semigroupTest $ nonEmptyMaker
    , testProperties "ThisOrThat Semigroup test" . semigroupTest $
        thisOrThatMaker
    , testProperties "Name Monoid test" . monoidTest $ nameGenerator
    , testProperties "Endo Monoid test" . endoMonoidTest $ intGenerator
    ]
