{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TransformListComp   #-}

module ConcurrentHashTableSpec
  ( hashTableTests
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall (..), throwTo)
import Control.Monad (forM, forM_, replicateM)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, isNothing)
import GHC.Exts (groupWith, the)
import Hedgehog (Property, PropertyT, assert, diff, evalIO, forAll, property, (===))
import qualified Hedgehog.Gen as Gen (int, list, lower)
import qualified Hedgehog.Range as Range (constant, singleton)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import ConcurrentHashTable (ConcurrentHashTable, getCHT, newCHT, putCHT, sizeCHT)


hashTableTests :: TestTree
hashTableTests = testGroup "concurrent hashtable"
  $ map (uncurry testProperty)
  [ ("single thread correctness test", monoCorrectness)
  , ("multi  thread correctness test", multiCorrectness)
  , ("asynchronous exceptions test",  asynchronousExceptionCorrectness)
  ]

requestAmount :: Int
requestAmount = 400

threadAmount :: Int
threadAmount = 4

check
  :: (ConcurrentHashTable Char Int -> [(Char, Int)] -> IO ())
  -> PropertyT IO (ConcurrentHashTable Char Int, [(Char, [Int])])
check setter = do
  let mkList = Gen.list $ Range.singleton requestAmount
  keys   <- forAll . mkList $ Gen.lower
  values <- forAll . mkList . Gen.int $ Range.constant 0 10
  let pairs = zip keys values
  cht <- evalIO newCHT
  evalIO $ setter cht pairs
  let results = [(the k, v) | (k, v) <- pairs, then group by k using groupWith]
  return (cht, results)

checkCorrectness
  :: (ConcurrentHashTable Char Int -> [(Char, Int)] -> IO ())
  -> Property
checkCorrectness setter = property do
  (cht, results) <- check setter
  evalIO (sizeCHT cht) >>= (=== length results)
  mapM_ (\(expected, real) -> diff real elem $ map pure expected) =<< evalIO
    (forM results \(k, vs) -> (,) vs <$> getCHT k cht)

monoCorrectness :: Property
monoCorrectness = checkCorrectness \cht -> mapM_ \(k, v) -> putCHT k v cht

multiCorrectness :: Property
multiCorrectness = checkCorrectness \cht pairs -> do
  let chunks = chunksOf (div requestAmount threadAmount) pairs
  vars <- replicateM (length chunks) newEmptyMVar
  let parts = zip vars chunks
  forM_ parts \(var, chunk) -> forkIO do
    forM_ chunk \(k, v) -> putCHT k v cht
    putMVar var ()
  mapM_ takeMVar vars

asynchronousExceptionCorrectness :: Property
asynchronousExceptionCorrectness = property do
  (cht, results) <- check \cht pairs -> do
    let chunks = chunksOf (div requestAmount threadAmount) pairs
    ids <- forM chunks \chunk -> forkIO $ forM_ chunk \(k, v) -> putCHT k v cht
    forM_ ids . flip throwTo $ ErrorCall "unexpected, huh?"
  size    <- evalIO $ sizeCHT cht
  reality <- evalIO $ forM results \(k, vs) -> (,) vs <$> getCHT k cht
  let valid exps rl = assert $ isNothing rl || rl `elem` map pure exps
  forM_ reality $ uncurry valid
  length (filter (isJust . snd) reality) === size
