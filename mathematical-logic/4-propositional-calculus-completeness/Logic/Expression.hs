module Logic.Expression where

import Data.Function (on)
import Data.Bits     (xor, complement, testBit)
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.HashSet as HSet (singleton, union, toList)
import qualified Data.HashMap.Strict as HMap (HashMap, fromList, (!))


data Expression = BinaryOperator Operator Expression Expression
                | Not            Expression
                | Variable       String
                deriving Eq

data Operator = And
              | Or
              | Implication
              deriving Eq

infixl 3 *->*
infixr 4 *|*
infixr 5 *&*

(*->*) :: Expression -> Expression -> Expression
(*|*)  :: Expression -> Expression -> Expression
(*&*)  :: Expression -> Expression -> Expression

(*->*) = (BinaryOperator Implication)
(*|*)  = (BinaryOperator Or)
(*&*)  = (BinaryOperator And)

instance Show Expression where
    show (BinaryOperator op expl expr) = "(" ++ unwords [show expl, show op, show expr] ++ ")"
    show (Not expr)                    = '!' : show expr
    show (Variable str)                = str

instance Show Operator where
    show Implication = "->"
    show Or          = "|"
    show And         = "&"

instance Hashable Expression where
    hash (BinaryOperator op l r) = (hash op) `xor` on xor hash l r
    hash (Not e)                 = complement . hash $ e
    hash (Variable str)          = hash str
    hashWithSalt salt ex         = salt `xor` (hash ex)

instance Hashable Operator where
    hash Implication     = 1
    hash And             = 4
    hash Or              = 16
    hashWithSalt salt op = salt `xor` (hash op)

variablesList :: Expression -> [String]
variablesList = HSet.toList . variablesSet
    where
        variablesSet (BinaryOperator _ l r) = on HSet.union variablesSet l r
        variablesSet (Not expr)             = variablesSet expr
        variablesSet (Variable str)         = HSet.singleton str

getValues :: Expression -> ([String], [Bool])
getValues expr = (vars, foldr transform [] [0..(2 ^ (length vars) - 1)])
    where
        transform :: Int -> [Bool] -> [Bool]
        transform num = ((checkExpression (HMap.fromList $ zip vars $ map (testBit num) [0..(length vars - 1)]) expr) :)
        vars          = variablesList expr

checkExpression :: (HMap.HashMap String Bool) -> Expression -> Bool
checkExpression vals (BinaryOperator Implication l r) = checkExpression vals $ BinaryOperator Or (Not l) r
checkExpression vals (BinaryOperator And l r)         = on (&&) (checkExpression vals) l r
checkExpression vals (BinaryOperator Or l r)          = on (||) (checkExpression vals) l r
checkExpression vals (Variable str)                   = vals HMap.! str
checkExpression vals (Not expr)                       = not $ checkExpression vals expr
