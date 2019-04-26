module Logic.Parsing.ExpressionParser where

import Logic.Parsing.Tokens
import Logic.Parsing.ExpressionGrammar
import Logic.Expression

getExpression :: String -> Expression
getExpression = parseExpression . alexScanTokens
