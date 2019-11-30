module Lambda.Parse.ExpressionParser (getExpression) where

import Lambda.Parse.Tokens
import Lambda.Parse.ExpressionGrammar
import Lambda.Expression

getExpression :: String -> Expression
getExpression = parseExpression . alexScanTokens
