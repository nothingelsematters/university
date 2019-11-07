module Lambda.Expression where

import Data.Function (on)
import qualified Data.Set as Set (Set, empty)
import qualified Data.Map.Strict as Map (Map, empty)


data Expression = Variable    { name        :: String }
                | Application { left        :: Expression, right      :: Expression }
                | Lambda      { variable    :: String,     expression :: Expression }
                deriving (Eq, Ord)

instance Show Expression where
    show (Variable    str) = str
    show (Application l r) = withParentheses $ show l ++ " " ++ show r
    show (Lambda var expr) = withParentheses $ "\\" ++ var ++ "." ++ show expr

withParentheses :: String -> String
withParentheses value = "(" ++ value ++  ")"


reduce :: Expression -> Expression
reduce (Application (Lambda v l) r) = substitute v l r
reduce (Application l r)            = on Application reduce l r
reduce expr                         = expr

substitute :: String -> Expression -> Expression -> Expression
substitute s (Application expr expr') r = Application (substitute s expr r) (substitute s expr' r)
substitute s (Lambda v l) r             = Lambda v $ substitute s l r
substitute s (Variable v) r             = if (s == v) then r else Variable v

conversion :: Set.Set String -> Set.Set String -> Set.Set String -> Expression -> Expression
conversion vars closed tops (Variable    str) = Variable $ getName vars str closed tops
conversion vars closed tops (Lambda var expr) = Lambda (if (Set.member var vars) then var ++ "passplease" else var)
    $ conversion vars (Set.insert var closed) tops expr
conversion vars closed tops (Application l r) = on Application (conversion vars closed tops) l r

{-
    все свободные переменные из начального терма после подстановки должны оставаться свободными
-}

getVariables :: Expression -> Set.Set String
getVariables (Variable    str) = Set.singleton str
getVariables (Application l r) = on Set.union getVariables l r
getVariables (Lambda var expr) = Set.insert var $ getVariables expr

getName :: Set.Set String -> String -> Set.Set String -> Set.Set String -> String
getName l r closed tops
    | Set.notMember r closed && Set.member r tops = r
    | Set.member r l = r ++ "passplease"
    | otherwise = r

getLambdaVariables :: Expression -> Set.Set String
getLambdaVariables (Variable      _) = Set.empty
getLambdaVariables (Application l r) = on Set.union getLambdaVariables l r
getLambdaVariables (Lambda var expr) = Set.insert var $ getLambdaVariables expr

getTopNames :: Expression -> Set.Set String
getTopNames expr = getVariables expr Set.\\ getLambdaVariables expr
