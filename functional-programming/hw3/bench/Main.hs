module Main
  ( main
  ) where

import Criterion.Main (defaultMain)

import ConcurrentHashTableBench (hashTableBenchmarks)
import GeometryBench (geometryBenchmarks)


main :: IO ()
main = defaultMain =<< sequence [geometryBenchmarks, hashTableBenchmarks]
