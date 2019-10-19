module Lambda.Expression where

data Expression = Variable    { name        :: String }
                | Application { left        :: Expression, right      :: Expression }
                | Lambda      { variable    :: String,     expression :: Expression }

instance Show Expression where
    show (Variable    str) = str
    show (Application l r) = withParentheses $ show l ++ " " ++ show r
    show (Lambda var expr) = withParentheses $ "\\" ++ var ++ "." ++ show expr

withParentheses :: String -> String
withParentheses value = "(" ++ value ++  ")"
