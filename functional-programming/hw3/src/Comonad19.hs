---------------------------------------------------------------------------
-- |
-- Module      : Comonad19
-- Description : Interactive epidemic simulation
-- Copyright   : (c) Simon Naumov, 2020
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
-- Language    : Haskell2010
--
-- Interactive parametrized epidemic simulation using zipper and 'Comonad'
-- concepts.
---------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Comonad19
  ( -- * Comonad-19
    Plague
  , explore
  , plague
  , step

   -- * Configuration
  , PlagueConfig (..)
  , getDefaultConfig
  ) where

import Control.Comonad (Comonad (..), (<<=))
import Control.Monad (liftM2)
import Data.Tuple (swap)
import System.Random (RandomGen (..), StdGen, getStdGen, randomR)


data ListZipper a = ListZipper [a] a [a]

listLeft :: ListZipper a -> ListZipper a
listLeft (ListZipper (a:as) x bs) = ListZipper as a (x:bs)
listLeft _                        = error "listLeft on out of bound"

listRight :: ListZipper a -> ListZipper a
listRight (ListZipper as x (b:bs)) = ListZipper (x:as) b bs
listRight _                        = error "listRight on out of bound"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (ListZipper ls _ rs) = ListZipper ls x rs

toList :: Int -> ListZipper a -> [a]
toList n (ListZipper ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

move :: (a -> a) -> (a -> a) -> a -> ListZipper a
move f g e = ListZipper (iterateTail f e) e (iterateTail g e)
  where
    iterateTail :: (a -> a) -> a -> [a]
    iterateTail func = tail . iterate func

instance Functor ListZipper where
  fmap f (ListZipper as x bs) = ListZipper (map f as) (f x) (map f bs)

instance Comonad ListZipper where
  extract (ListZipper _ x _) = x

  duplicate = move listLeft listRight


newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

gridUp :: Grid a -> Grid a
gridUp (Grid g) = Grid $ listLeft g

gridDown :: Grid a -> Grid a
gridDown (Grid g) = Grid $ listRight g

gridLeft :: Grid a -> Grid a
gridLeft (Grid g) = Grid $ fmap listLeft g

gridRight :: Grid a -> Grid a
gridRight (Grid g) = Grid $ fmap listRight g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite (listWrite x $ extract g) g

gridModify :: (a -> a) -> Grid a -> Grid a
gridModify f g = gridWrite (f $ extract g) g

instance Functor Grid where
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
  extract   = extract . extract . unGrid

  duplicate = Grid . fmap (move gridLeft gridRight) . move gridUp gridDown


data Stage
  = Health
  | Incubation Int
  | Disease    Int
  | Immunity   Int

instance Show Stage where
  show  Health        = " "
  show (Incubation _) = "."
  show (Disease    _) = "#"
  show (Immunity   _) = "@"

data Cell r
  = Cell
  { randGen :: r
  , stage   :: Stage
  }

instance Show (Cell r) where
  show = show . stage

-- | A data type that carries configuration and grid with cells.
data Plague r = Plague (PlagueConfig r) (Grid (Cell r))

-- | Plague configuration.
data PlagueConfig r
  = PlagueConfig
  { infectionProbability :: Double -- ^ Probability to get contact infection.
  , incubationPeriod     :: Int    -- ^ Incubation period of a virus.
  , diseasePeriod        :: Int    -- ^ Period of someone's disease.
  , immunityPeriod       :: Int    -- ^ Period of immunity after disease.
  , initRandGen          :: r      -- ^ Initial random generator.
  }

-- | Get default configuration.
getDefaultConfig :: IO (PlagueConfig StdGen)
getDefaultConfig = do
  gen <- getStdGen
  return PlagueConfig
    { infectionProbability = 0.1
    , incubationPeriod     = 10
    , diseasePeriod        = 20
    , immunityPeriod       = 10
    , initRandGen          = gen
    }

validateConfig :: PlagueConfig r -> Either String (PlagueConfig r)
validateConfig gc
  | (< 0) $ incubationPeriod gc = Left "infection period must be positive"
  | (< 0) $    diseasePeriod gc = Left "disease period must be positive"
  | (< 0) $   immunityPeriod gc = Left "immunity period must be positive"
  | liftM2 (||) (< 0) (> 1) $ infectionProbability gc =
      Left "infection probability must be greater that zero and less than one"
  | otherwise = Right gc

infected :: Cell r -> Bool
infected = infectedStage . stage
  where
    infectedStage :: Stage -> Bool
    infectedStage (Incubation _) = True
    infectedStage (Disease    _) = True
    infectedStage  _             = False

newCell :: RandomGen r => r -> Cell r
newCell gen = Cell
  { randGen = gen
  , stage   = Health
  }

neighbours :: forall a . [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where
    horizontals :: [Grid a -> Grid a]
    horizontals = [gridLeft, gridRight]

    verticals :: [Grid a -> Grid a]
    verticals = [gridUp, gridDown]

-- | Create an initial world with one infected person.
plague :: forall r . RandomGen r => PlagueConfig r -> Either String (Plague r)
plague config@PlagueConfig{..} = do
  conf <- validateConfig config
  return . Plague conf . gridModify source . Grid $
    makeZipper makeGrid makeRow initRandGen
  where
    makeZipper :: RandomGen r => (r -> [a]) -> (r -> a) -> r -> ListZipper a
    makeZipper side center gen = ListZipper (side l) (center gen) (side r)
      where
        (l, r) = split gen

    source :: RandomGen r => Cell r -> Cell r
    source c = c{ stage = Incubation incubationPeriod }

    makeCells :: RandomGen r => r -> [Cell r]
    makeCells gen = map newCell . flip iterate gen $ snd . split

    makeRow :: RandomGen r => r -> ListZipper (Cell r)
    makeRow = makeZipper makeCells newCell

    makeGrid :: RandomGen r => r -> [ListZipper (Cell r)]
    makeGrid gen = map makeRow . flip iterate gen $ snd . split

infect :: forall r . RandomGen r => Double -> r -> Grid (Cell r) -> (r, Bool)
infect infectionProb gen g = meetInfected . length . filter infected $
  map (extract . ($ g)) neighbours
  where
    meetInfected :: RandomGen r => Int -> (r, Bool)
    meetInfected 0 = (gen, False)
    meetInfected n = getInfected $! meetInfected (n - 1)

    getInfected :: RandomGen r => (r, Bool) -> (r, Bool)
    getInfected (p, l) = (|| l) . (<= infectionProb) <$> swap (randomR (0, 1) p)

rule :: forall r . RandomGen r => PlagueConfig r -> Grid (Cell r) -> Cell r
rule PlagueConfig{..} g = case stage cell of
  Incubation 0 -> cell{ stage = Disease diseasePeriod }
  Incubation n -> cell{ stage = Incubation (n - 1) }
  Disease    0 -> cell{ stage = Immunity immunityPeriod }
  Disease    n -> cell{ stage = Disease (n - 1) }
  Immunity   0 -> cell{ stage = Health }
  Immunity   n -> cell{ stage = Immunity (n - 1) }
  Health       -> cell{ randGen = fst contacted, stage = snd contacted }
  where
    cell :: RandomGen r => Cell r
    cell = extract g

    getInfection :: Bool -> Stage
    getInfection True  = Incubation incubationPeriod
    getInfection False = Health

    contacted :: (r, Stage)
    contacted = getInfection <$> infect infectionProbability (randGen cell) g

-- | To reproduce a single step in the simulation.
step :: RandomGen r => Plague r -> Plague r
step (Plague conf g) = Plague conf $ rule conf <<= g

-- | To print a grid of a simulation world. "#" stands for infected, "@" for
-- recovered, " " for a healthy non-immune person and "." for immune.
explore :: Int -> Plague a -> String
explore n (Plague _ g) =
  unlines . map (>>= show) . toList n . fmap (toList n) $ unGrid g
