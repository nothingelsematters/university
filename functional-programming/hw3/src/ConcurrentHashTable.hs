----------------------------------------------------------------
-- |
-- Module      : ConcurrentHashTable
-- Description : concurrent hash table
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
-- Language    : Haskell2010
--
-- Fine grained concurrent hash table algorithm implementation.
----------------------------------------------------------------
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TransformListComp #-}

module ConcurrentHashTable
  ( ConcurrentHashTable
  , KeyConstraints
  , getCHT
  , newCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (mask, onException)
import Control.Monad (replicateM, unless, when)
import Data.Bits (shiftL)
import Data.Function (on)
import Data.Hashable (Hashable, hash)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import Data.List (deleteBy)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector (fromList, length, mapM, replicateM, toList)
import GHC.Exts (groupWith, the)


-- | Constraints for keys in this certain hashtable implementation.
type KeyConstraints k = (Eq k, Hashable k)

type Buckets k v = Vector (MVar [(k, v)])

-- | Concurrent hashtable implementation using fine grained algorithm.
data ConcurrentHashTable k v
  = ConcurrentHashTable
  { chtBuckets :: IORef (Buckets k v)
  , chtSize    :: IORef Int
  , chtGrowing :: IORef Bool
  }

loadFactor :: Double
loadFactor = 0.75

initialSize :: Int
initialSize = 128

getBucket
  :: KeyConstraints k
  => ConcurrentHashTable k v
  -> k
  -> IO ([(k, v)], MVar [(k, v)])
getBucket table key = do
  currentBuckets <- readIORef $ chtBuckets table
  let currentLength  = Vector.length currentBuckets
  let bucketVariable = currentBuckets ! mod (hash key) currentLength

  mask \restore -> do
    bucket <- takeMVar bucketVariable
    let exceptionOccurred = putMVar bucketVariable bucket
    flip onException exceptionOccurred $ restore do
      currentSize <- readIORef $ chtSize table

      if on (/) realToFrac currentSize currentLength < loadFactor
      then return (bucket, bucketVariable)
      else do
        putMVar bucketVariable bucket
        tryResizing table currentLength
        getBucket table key

tryResizing :: KeyConstraints k => ConcurrentHashTable k v -> Int -> IO ()
tryResizing ConcurrentHashTable{..} oldLength = do
  t <- readIORef chtGrowing
  unless t do
    ok <- atomicModifyIORef' chtGrowing $ const (True, True)
    when ok do
      oldVector <- readIORef chtBuckets
      when (oldLength == Vector.length oldVector) $
        resize oldVector chtBuckets $ oldLength `shiftL` 1
      atomicWriteIORef chtGrowing False

resize :: KeyConstraints k => Buckets k v -> IORef (Buckets k v) -> Int -> IO ()
resize oldBuckets bucketsVariable newLength = do
  pairs <- fmap (concat . Vector.toList) . Vector.mapM readMVar $ oldBuckets
  newVector <- Vector.replicateM newLength $ newMVar []
  mapM_ (\(ind, vs) -> modifyMVar_ (newVector ! ind) (const $ return vs))
    [ (the hsh, v)
    | v <- pairs
    , let hsh = flip mod newLength . hash $ fst v
    , then group by hsh using groupWith
    ]
  atomicWriteIORef bucketsVariable newVector


-- | Create a new hash table.
newCHT  :: KeyConstraints k => IO (ConcurrentHashTable k v)
newCHT = do
  rawBuckets <- replicateM initialSize $ newMVar []
  newBuckets <- newIORef $ Vector.fromList rawBuckets
  newSize    <- newIORef 0
  newGrowing <- newIORef False
  return $! ConcurrentHashTable
    { chtBuckets = newBuckets
    , chtSize    = newSize
    , chtGrowing = newGrowing
    }

-- | Get an associated to a key value stored in the table.
getCHT  :: KeyConstraints k => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key table = do
  bcks  <- readIORef $ chtBuckets table
  fmap (lookup key) . readMVar . (bcks !) . mod (hash key) $ Vector.length bcks

-- | Put some value associated to a key into a table.
putCHT  :: KeyConstraints k => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value table = mask \restore -> do
  (bucket, bucketVariable) <- getBucket table key
  let exceptionOccurred = putMVar bucketVariable bucket
  flip onException exceptionOccurred $ restore do
    let clearBucket = deleteBy (on (==) fst) (key, undefined) bucket
    putMVar bucketVariable . ((key, value) :) $! clearBucket
    when (on (==) length bucket clearBucket) $
      atomicModifyIORef' (chtSize table) \a -> (a + 1, ())

-- | Get the current tabel size.
sizeCHT :: KeyConstraints k => ConcurrentHashTable k v -> IO Int
sizeCHT = readIORef . chtSize
