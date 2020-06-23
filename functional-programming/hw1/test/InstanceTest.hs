{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module InstanceTest
  ( applicativeTest
  , applyMaker
  , foldableTest
  , functorTest
  , monoidTest
  , semigroupTest
  , simpleTests
  , testProperties
  , traversableTest
  ) where

import Data.Foldable (fold)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Dual (..), Endo (..), Sum (..), appEndo, getDual, getSum)

import Generator (intGenerator, listGenerator)

import Hedgehog (Gen, Property, PropertyT, assert, forAll, property, (===))
import Hedgehog.Function (fn, forAllFn)
import qualified Hedgehog.Gen as Gen (list)
import qualified Hedgehog.Range as Range (constant)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)


testProperties :: TestName -> [(TestName, Property)] -> TestTree
testProperties name = testGroup name . map (uncurry testProperty)

simpleTests :: TestName -> [(TestName, Bool)] -> TestTree
simpleTests name = testProperties name . map (fmap (property . assert))

type TestMaker a = PropertyT IO a -> Property

applyMaker
  :: PropertyT IO a
  -> [(TestName, (PropertyT IO a -> b))]
  -> [(TestName, b)]
applyMaker maker = map (fmap ($ maker))

simpleRuleTest :: (a -> PropertyT IO ()) -> TestMaker a
simpleRuleTest rule maker = property $ maker >>= rule

foldRuleTest
  :: Foldable f
  => ((Int -> Int -> Int) -> Int -> f Int -> PropertyT IO ())
  -> TestMaker (f Int)
foldRuleTest rule foldableMaker = property $ do
  foldable <- foldableMaker
  initial <- forAll intGenerator
  rule (-) initial foldable

endoTest :: Foldable f => TestMaker (f Int)
endoTest = foldRuleTest $ \mapping initial foldable ->
  foldr mapping initial foldable ===
  appEndo (foldMap (Endo . mapping) foldable) initial

dualTest :: Foldable f => TestMaker (f Int)
dualTest = foldRuleTest $ \mapping initial foldable ->
  foldl mapping initial foldable ===
  appEndo (getDual (foldMap (Dual . Endo . flip mapping) foldable)) initial

foldMapIdTest
  :: (Foldable f, Monoid m, Eq m, Show m)
  => PropertyT IO (f m)
  -> Property
foldMapIdTest foldableGen = property $ foldableGen >>= \f ->
  fold f === foldMap id f

lengthTest :: Foldable f => TestMaker (f a)
lengthTest = simpleRuleTest $ \foldable ->
  length foldable === (getSum . foldMap (Sum . const  1) $ foldable)

sumTest :: Foldable f => TestMaker (f Int)
sumTest = simpleRuleTest $ \foldable ->
  sum foldable === (getSum . foldMap Sum $ foldable)

foldableTest
  :: (Foldable f, Monoid m, Eq m, Show m)
  => PropertyT IO (f Int)
  -> PropertyT IO (f m)
  -> [(TestName, Property)]
foldableTest intMaker monoidMaker =
  ("rule \"fold = foldMap id\"", foldMapIdTest monoidMaker) :
  applyMaker intMaker
  [ ("rule \"length = getSum . foldMap (Sum . const  1)\"", lengthTest)
  , ("rule \"sum = getSum . foldMap Sum\"", sumTest)
  , ("rule \"foldr f z t = appEndo (foldMap (Endo . f) t ) z\"", endoTest)
  , ( "rule \"foldl f z t = \
      \appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z\""
    , dualTest
    )
  ]


associativityTest :: (Eq s, Show s, Semigroup s) => TestMaker s
associativityTest maker = property $ do
  x <- maker
  y <- maker
  z <- maker
  x <> (y <> z) === (x <> y) <> z

semigroupTest
  :: (Eq s, Show s, Semigroup s)
  => PropertyT IO s
  -> [(TestName, Property)]
semigroupTest maker =
  [("rule \"x <> (y <> z) = (x <> y) <> z\"", associativityTest maker)]


rightMemptyTest :: (Monoid m, Eq m, Show m) => TestMaker m
rightMemptyTest = simpleRuleTest $ \x -> x <> mempty === x

leftMemptyTest :: (Monoid m, Eq m, Show m) => TestMaker m
leftMemptyTest = simpleRuleTest $ \x -> mempty <> x === x

mconcatTest :: (Monoid m, Eq m, Show m) => Gen m -> Property
mconcatTest generator = property $ do
  list <- forAll . listGenerator $ generator
  mconcat list === foldr (<>) mempty list

monoidTest :: (Eq m, Show m, Monoid m) => Gen m -> [(TestName, Property)]
monoidTest generator = semigroupTest maker ++
  ("rule \"mconcat = foldr (<>) mempty\"", mconcatTest generator) :
  applyMaker maker
  [ ("rule \"x <> mempty = x\"", rightMemptyTest)
  , ("rule \"mempty <> x = x\"", leftMemptyTest)
  ]
    where
      maker = forAll generator


fmapIdTest :: (Functor f, Eq (f a), Show (f a)) => TestMaker (f a)
fmapIdTest = simpleRuleTest $ \x -> fmap id x === x

fmapCompositionTest
  :: (Functor f, Eq (f Int), Show (f Int))
  => TestMaker (f Int)
fmapCompositionTest maker = property $ do
  f <- forAllFn $ fn @Int intGenerator
  g <- forAllFn $ fn @Int intGenerator
  x <- maker
  fmap (f . g) x === (fmap f . fmap g) x

functorTest
  :: (Functor f, Eq (f Int), Show (f Int))
  => PropertyT IO (f Int)
  -> [(TestName, Property)]
functorTest maker = applyMaker maker
  [ ("rule \"fmap id  ==  id\"", fmapIdTest)
  , ("rule \"fmap (f . g)  ==  fmap f . fmap g\"", fmapCompositionTest)
  ]


identityTest :: (Applicative f, Eq (f a), Show (f a)) => TestMaker (f a)
identityTest = simpleRuleTest $ \v -> (pure id <*> v) === v

compositionTest
  :: (Applicative f, Eq (f Int), Show (f Int))
  => TestMaker (f Int)
compositionTest maker = property $ do
  u <- fmap pure . forAllFn $ fn @Int intGenerator
  v <- fmap pure . forAllFn $ fn @Int intGenerator
  w <- maker
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

homomorphismTest
  :: forall f . (Applicative f, Eq (f Int), Show (f Int))
  => PropertyT IO Int
  -> TestMaker (f Int)
homomorphismTest maker _ = property $ do
  f <- forAllFn $ fn @Int intGenerator
  x <- maker
  (pure f <*> (pure :: Int -> f Int) x) === pure (f x)

interchangeTest
  :: forall f . (Applicative f, Eq (f Int), Show (f Int))
  => PropertyT IO Int
  -> TestMaker (f Int)
interchangeTest maker _ = property $ do
  u <- fmap (pure :: a -> f a) . forAllFn $ fn @Int intGenerator
  y <- maker
  (u <*> pure y) === (pure ($ y) <*> u)

applicativeTest
  :: (Applicative f, Eq (f Int), Show (f Int))
  => PropertyT IO (f Int)
  -> [(TestName, Property)]
applicativeTest maker =
  (functorTest maker ++) .
  applyMaker maker $
  applyMaker (forAll intGenerator)
  [ ("rule \"pure f <*> pure x = pure (f x)\"", homomorphismTest)
  , ("rule \"u <*> pure y = pure ($ y) <*> u\"", interchangeTest)
  ] ++
  [ ("rule \"pure id <*> v = v\"", identityTest)
  , ("rule \"pure (.) <*> u <*> v <*> w = u <*> (v <*> w)\"", compositionTest)
  ]

traverseNaturalityTest
  :: (Traversable t, Eq (t Int), Show (t Int))
  => TestMaker (t Int)
traverseNaturalityTest maker = property $ do
  x <- maker
  let f = pure :: a -> [a]
  let t = reverse
  (t . traverse f $ x) === traverse (t . f) x

traverseIdentityTest
  :: (Traversable t, Eq (t Int), Show (t Int))
  => TestMaker (t Int)
traverseIdentityTest = simpleRuleTest $ \x -> traverse Identity x === Identity x

traverseCompositionTest
  :: (Traversable t, Eq (t Int), Show (t Int))
  => TestMaker (t Int)
traverseCompositionTest maker = property $ do
  let f = pure :: a -> Maybe a
  let g = pure :: a -> [a]
  x <- maker
  traverse (Compose . fmap g . f) x ===
    (Compose . fmap (traverse g) . traverse f) x

sequenceNaturalityTest
  :: (Traversable t, Eq (t [Int]), Show (t [Int]))
  => TestMaker (t [[Int]])
sequenceNaturalityTest maker = property $ do
  let t = reverse
  element <- maker
  (t . sequenceA) element === (sequenceA . fmap t) element

sequenceIdentityTest
  :: (Traversable t, Eq (t Int), Show (t Int))
  => TestMaker (t Int)
sequenceIdentityTest = simpleRuleTest $ \x ->
  (sequenceA . fmap Identity) x === Identity x

sequenceCompositionTest
  :: (Traversable t, Eq (t Int), Show (t Int), Eq (t [[Int]]), Show (t [[Int]]))
  => TestMaker (t [[Int]])
sequenceCompositionTest = simpleRuleTest $ \x ->
  (sequenceA . fmap Compose) x === (Compose . fmap sequenceA . sequenceA) x

traversableTest
  :: forall t
   . ( Traversable t
     , Eq (t Int)
     , Show (t Int)
     , Eq (t [Int])
     , Show (t [Int])
     , Eq (t [[Int]])
     , Show (t [[Int]])
     )
  => (Gen Int -> Gen (t Int))
  -> (Gen [[Int]] -> Gen (t [[Int]]))
  -> [(TestName, Property)]
traversableTest generator listsGenerator = listsTests ++ intTests
  where
    smallListGenerator :: Gen a -> Gen [a]
    smallListGenerator = Gen.list (Range.constant 0 2)

    lists :: PropertyT IO (t [[Int]])
    lists = forAll . listsGenerator . smallListGenerator . smallListGenerator $
      intGenerator

    listsTests :: [(TestName, Property)]
    listsTests = applyMaker lists
      [ ( "rule \"sequenceA . fmap Compose = \
          \Compose . fmap sequenceA . sequenceA\""
        , sequenceCompositionTest
        )
      , ( "rule \"t . sequenceA = sequenceA . fmap t\""
        , sequenceNaturalityTest
        )
      ]

    intTests :: [(TestName, Property)]
    intTests = applyMaker (forAll $ generator intGenerator)
      [ ("rule \"t . traverse f = traverse (t . f)\"", traverseNaturalityTest)
      , ("rule \"traverse Identity = Identity\"", traverseIdentityTest)
      , ( "rule \"traverse (Compose . fmap g . f) = \
          \Compose . fmap (traverse g) . traverse f\""
        , traverseCompositionTest
        )
      , ("rule \"sequenceA . fmap Identity = Identity\"", sequenceIdentityTest)
      ]
