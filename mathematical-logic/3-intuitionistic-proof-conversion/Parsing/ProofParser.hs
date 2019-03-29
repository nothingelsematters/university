module Parsing.ProofParser where

import Parsing.Tokens
import Parsing.TaskGrammar
import Parsing.ExpressionGrammar
import Expression
    
getTask :: String -> Task
getTask = parseTask . alexScanTokens

getExpression :: String -> Expression
getExpression = parseExpression . alexScanTokens