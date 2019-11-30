{
module Lambda.Parse.ExpressionGrammar (parseExpression) where

import Lambda.Parse.Tokens
import Lambda.Expression
}

%name parseExpression
%tokentype { Token }
%error { parseError }

%token
    VARIABLE { TokenVariable $$ }
    OPENP    { TokenOP }
    CLOSEP   { TokenCP }
    DOT      { TokenDot }
    LAMBDA   { TokenLambda }
%%

Expression : Application                { $1 }
           | Application Lambda         { Application $1 $2 }
           | Lambda                     { $1 }

Lambda : LAMBDA VARIABLE DOT Expression { Lambda $2 $4 }

Application : Application Atom          { Application $1 $2 }
            | Atom                      { $1 }

Atom : OPENP Expression CLOSEP          { $2 }
     | VARIABLE                         { Variable $1 }

{
parseError :: [Token] -> a
parseError e = error $ "Parse error " ++ (show e)
}
