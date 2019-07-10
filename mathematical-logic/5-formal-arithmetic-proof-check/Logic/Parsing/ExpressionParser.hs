module Logic.Parsing.ExpressionParser
     ( getExpression
     , getTask
     ) where

import Logic.Parsing.Tokens
import Logic.Parsing.ExpressionGrammar
import Logic.Parsing.TaskGrammar
import Logic.Expression
import Logic.Proof

getExpression :: String -> Expression
getExpression = parseExpression . alexScanTokens

getTask :: String -> Task
getTask = parseTask . alexScanTokens
