module Expression (Expression (..), Operator (..)) where

import Data.List (intercalate)

data Expression = BinaryOperator Operator Expression Expression
                | Not Expression
                | Variable String

data Operator = And | Or | Implication

instance Show Expression where
    show (BinaryOperator op expl expr) = inBrackets $
                                intercalate "," [show op, show expl, show expr]
    show (Not exp) = inBrackets $ '!' : show exp
    show (Variable str) = str

instance Show Operator where
    show Implication = "->"
    show Or = "|"
    show And = "&"

inBrackets x = concat ["(", x, ")"]
