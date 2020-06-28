{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE ParallelListComp #-}

module ConcurrentHashTableBench
  ( hashTableBenchmarks
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar,takeMVar)
import Control.Monad (forM_, replicateM, void)
import Criterion.Main (Benchmark, bench, bgroup, nfAppIO)
import Data.Hashable (Hashable)
import Data.List.Split (chunksOf)

import ConcurrentHashTable (ConcurrentHashTable, getCHT, newCHT, putCHT, sizeCHT)


hashTableBenchmarks :: IO Benchmark
hashTableBenchmarks = return $ bgroup "hashtable"
  [ bench (balanceName <> "/" <> show (length req) <> "/" <> name) $
      nfAppIO act req
  | (balanceName, b) <- balances
  , s <- sizes
  , let !req = requestSupplier s b
  , (name, act) <- performers
  ]

requestSupplier :: [b] -> [Int] -> [Request String b]
requestSupplier b types =
  [ toRequest n l r
  | l <- [[chl, chr] | chl <- ['A'..'z'], chr <- ['A'..'z']]
  , r <- b
  | n <- cycle types
  ]

toRequest :: Int -> a -> b -> Request a b
toRequest 0 k _ = Get k
toRequest 1 k v = Put k v
toRequest 2 _ _ = Size
toRequest _ _ _ = error "there are three requests"

data Request k v
  = Get k
  | Put k v
  | Size

sizes :: [[Int]]
sizes = [[0..100], [0..1000]]

balances :: [(String, [Int])]
balances =
  [ ("balanced", [0..2])
  , (">put",     replicate 5  1 ++ [0, 2])
  , (">get",     replicate 10 0 ++ [1, 2])
  , ("puts",     [1])
  , ("gets",     [0])
  ]

performers :: (Eq k, Hashable k) => [(String, [Request k v] -> IO ())]
performers = map (fmap \x y -> newCHT >>= flip x y)
  [ ("mono",   mono)
  , ("stereo", multi 2)
  , ("quadro", multi 4)
  ]

mono :: (Eq k, Hashable k) => ConcurrentHashTable k v -> [Request k v] -> IO ()
mono cht requests = forM_ requests \case
  Get k   -> void $ getCHT k cht
  Put k v -> putCHT k v cht
  Size    -> void $ sizeCHT cht

multi
  :: (Eq k, Hashable k)
  => Int
  -> ConcurrentHashTable k v
  -> [Request k v]
  -> IO ()
multi n cht requests = do
  let chunks = chunksOf ((length requests `div` n) + 1) requests
  vars <- replicateM (length chunks) newEmptyMVar
  forM_ (zip vars chunks) \(var, chunk) -> forkIO do
    mono cht chunk
    putMVar var undefined
  mapM_ takeMVar vars

