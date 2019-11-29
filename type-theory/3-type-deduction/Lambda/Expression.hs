module Lambda.Expression where

data Expression = Variable    { name        :: String }
                | Application { left        :: Expression, right      :: Expression }
                | Lambda      { variable    :: String,     expression :: Expression }
                deriving (Eq, Ord)

infix 5 *.*
(*.*) :: Expression -> Expression -> Expression
(*.*) = Application

infix 6 *\*
(*\*) :: String -> Expression -> Expression
(*\*) = Lambda

instance Show Expression where
    show (Variable    str) = str
    show (Application l r) = withParentheses $ show l ++ " " ++ show r
    show (Lambda var expr) = withParentheses $ "\\" ++ var ++ "." ++ show expr


withParentheses :: String -> String
withParentheses value = "(" ++ value ++  ")"
