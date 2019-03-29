{
module Parsing.TaskGrammar (parseTask) where

import Parsing.Tokens
import Expression
}

%name parseTask
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
      TOURNIQUET             { TokenTourniquet }
      COMA                   { TokenComa }

%%

Task : Hypotheses TOURNIQUET Expression      { Task $1 $3 }
     | TOURNIQUET Expression                 { Task [] $2 }

Hypotheses : Expression COMA Hypotheses      { $1 : $3 }
           | Expression                      { [$1] }

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
