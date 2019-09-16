module Logic.Expression where

import Data.Function (on)

data Expression = BinaryOperator Operator Expression Expression
                | Not            Expression
                | Quantifiers    Quantifier String Expression
                | Predication    PSymbol
                deriving (Eq, Ord, Show)

data PSymbol    = Equals      Therm Therm
                | Predicate   String [Therm]
                deriving (Eq, Ord, Show)

data Therm      = Application FSymbol
                | Variable    String
                deriving (Eq, Ord, Show)

data FSymbol    = Plus           Therm Therm
                | Multiplication Therm Therm
                | Colon          Therm
                | Function       String [Therm]
                | Zero
                deriving (Eq, Ord, Show)

data Operator   = And
                | Or
                | Implication
                deriving (Eq, Ord, Show)

data Quantifier = Forall
                | Exists
                deriving (Eq, Ord, Show)

checkClosed :: String -> Expression -> Bool
checkClosed variable (BinaryOperator _ l r) = on (&&) (checkClosed variable) l r
checkClosed variable (Not e)                = checkClosed variable e
checkClosed variable (Quantifiers _ v e)    = v == variable || checkClosed variable e
checkClosed variable (Predication ps)       = checkClosedPS ps
    where
        checkClosedPS (Equals tl tr)         = checkTherms tl tr
        checkClosedPS (Predicate _ tl)       = checkAllTherms tl

        checkClosedTh (Application fs)       = checkClosedF fs
        checkClosedTh (Variable v)           = v /= variable

        checkClosedF  (Plus tl tr)           = checkTherms tl tr
        checkClosedF  (Multiplication tl tr) = checkTherms tl tr
        checkClosedF  (Colon t)              = checkClosedTh t
        checkClosedF  (Function _ tl)        = checkAllTherms tl
        checkClosedF   Zero                  = True

        checkTherms    = on (&&) checkClosedTh
        checkAllTherms = all checkClosedTh
