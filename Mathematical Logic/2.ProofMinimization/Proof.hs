module Proof where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map (Map)


data Expression = BinaryOperator Operator Expression Expression
                | Not            Expression
                | Variable       String
                deriving (Eq, Ord)

data Operator = And | Or | Implication deriving (Eq, Ord)

data Task = Task { hypotheses :: [Expression],
                   result     ::  Expression }

data HalfProof = HalfProof { hyp     :: Map.Map Expression Int,
                             res     :: Expression,
                             exps    :: Map.Map Expression (ExpressionType, Int),
                             parts   :: Map.Map Expression [(Expression, Int)],
                             size    :: Int,
                             failure :: Bool }

data SortedProof = SortedProof { task         :: Task,
                                 expressions  :: [(Expression, (ExpressionType, Int))] }

data ExpressionType = Hypothesis Int | Axiom Int | ModusPonens Int Int deriving (Eq, Ord)


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

instance Show ExpressionType where
    show (Hypothesis n) = "Hypothesis " ++ show n
    show (Axiom n) = "Ax. sch. " ++ show n
    show (ModusPonens x y) = "M.P. " ++ show x ++ ", " ++ show y

showExpression :: (Expression, (ExpressionType, Int)) -> Int -> String
showExpression (ex, (ty, _)) num = "[" ++ show num ++ ". " ++ show ty ++ "] " ++ show ex
