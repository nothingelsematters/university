module Expression where

import Data.List (intercalate)
import Data.Hashable
import Data.Bits

data Expression = BinaryOperator Operator Expression Expression
                | Not            Expression
                | Variable       String
                deriving Eq

data Operator = And | Or | Implication deriving Eq

data Task = Task { hypotheses :: [Expression],
                   result     ::  Expression }

instance Show Expression where
    show (BinaryOperator op expl expr) =
        "(" ++ intercalate " " [show expl, show op, show expr] ++ ")"
    show (Not exp) = '!' : show exp
    show (Variable str) = str

instance Show Operator where
    show Implication = "->"
    show Or = "|"
    show And = "&"

instance Show Task where
    show t = showHypotheses ++ "|- " ++ (show . result $ t)
        where showHypotheses = case (intercalate ", ") . (map show) . hypotheses $ t of
                  "" -> ""
                  str -> str ++ " "
                  
instance Hashable Operator where
    hashWithSalt salt op = salt `xor` (hash' op)
        where hash' Implication = 1
              hash' And         = 4
              hash' Or          = 16

instance Hashable Expression where
    hashWithSalt salt ex = salt `xor` (hash' ex)
        where hash' (BinaryOperator op l r) = (hash op) `xor` (hash' l) `xor` (hash' r)
              hash' (Not e)                 = complement . hash' $ e
              hash' (Variable str)          = hash str
