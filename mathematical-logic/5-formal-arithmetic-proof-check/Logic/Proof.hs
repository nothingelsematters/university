module Logic.Proof where

import Data.Set         as Set (Set, fromList)
import Data.Map.Strict  as Map (Map, empty)
import Logic.Expression

data Task  = Task [Expression] Expression

data Proof = Proof { hypotheses  :: Set.Set Expression
                   , conclusion  :: Expression
                   , obtained    :: Bool
                   , indexes     :: Map.Map Expression Int
                   , modusPonens :: Map.Map Expression [(Expression, Int)]
                   , size        :: Int }

fromTask :: Task -> Proof
fromTask (Task leftside rightside) =
    Proof { hypotheses  = Set.fromList leftside
          , conclusion  = rightside
          , obtained    = False
          , indexes     = Map.empty
          , modusPonens = Map.empty
          , size        = 0 }
