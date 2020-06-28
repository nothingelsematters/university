{-# LANGUAGE ScopedTypeVariables #-}

module GeometryBench
  ( geometryBenchmarks
  ) where

import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.List.Split (chunksOf)
import System.Random (getStdGen, randoms)

import Geometry (Point (..), Polygon, distance, doubleArea, perimeter, subArea)


cyclicSumWith :: Num b => (a -> a -> b) -> [a] -> b
cyclicSumWith func xs = sum . zipWith func xs . drop 1 $ cycle xs

naivePerimeter :: Polygon -> Double
naivePerimeter = cyclicSumWith distance

naiveDoubleArea :: Polygon -> Int
naiveDoubleArea = cyclicSumWith subArea

polygons :: IO [(String, Polygon)]
polygons = do
  rs <- chunksOf (15 ^ (3 :: Int)) . randoms <$> getStdGen
  return
    [ ("10^5",        bounded 1e5)
    , ("10^6",        bounded 1e6)
    , ("10^7",        bounded 1e7)
    , ("random:10^7", points (head rs) $ rs !! 1)
    ]
  where
    points :: [Int] -> [Int] -> [Point]
    points xs ys = [Point a b | a <- xs, b <- ys]

    bounded :: Double -> [Point]
    bounded b = points boundedList boundedList
      where
        boundedList :: [Int]
        boundedList = [0..floor $ sqrt b]

race
  :: forall a b
  .  (NFData b)
  => String
  -> (a -> b)
  -> (a -> b)
  -> [(String, a)]
  -> Benchmark
race name naive original values = bgroup name
  [ bgroup testName
    [bench algo $ test `seq` nf func test | (algo, func) <- funcs]
  | (testName, test) <- values
  ]
  where
    funcs :: [(String, a -> b)]
    funcs =
      [ ("naive",    naive)
      , ("original", original)
      ]

functions :: [(String, Polygon)] -> Benchmark
functions testPolygons = bgroup "geometry" $ map ($ testPolygons)
  [ race "perimeter"  naivePerimeter  perimeter
  , race "doubleArea" naiveDoubleArea doubleArea
  ]

geometryBenchmarks :: IO Benchmark
geometryBenchmarks = functions <$> polygons
