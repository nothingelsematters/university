{
module Parsing.ExpressionGrammar (parseExpression) where

import Parsing.Tokens
import Expression
}

%name parseExpression
%tokentype { Token }
%error { parseError }

%token
      VARIABLE               { TokenVariable $$ }
      AND                    { TokenAnd }
      OR                     { TokenOr }
      IMPLICATION            { TokenImplication }
      NOT                    { TokenNot }
      OPENP                  { TokenOP }
      CLOSEP                 { TokenCP }

%%

Expression : OrPart IMPLICATION Expression   { BinaryOperator Implication $1 $3 }
           | OrPart                          { $1 }

OrPart : OrPart OR AndPart                   { BinaryOperator Or $1 $3 }
       | AndPart                             { $1 }

AndPart : AndPart AND FinalPart              { BinaryOperator And $1 $3 }
        | FinalPart                          { $1 }

FinalPart : VARIABLE                         { Variable $1 }
          | NOT FinalPart                    { Not $2 }
          | OPENP Expression CLOSEP          { $2 }


{
parseError :: [Token] -> a
parseError e = error $ "Parse error " ++ (show e)
}
