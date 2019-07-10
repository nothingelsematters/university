{
module Logic.Parsing.ExpressionGrammar (parseExpression) where

import Logic.Parsing.Tokens
import Logic.Expression
}

%name parseExpression
%tokentype { Token }
%error { parseError }

%token
      CAPVARIABLE            { TokenCapVariable $$ }
      SMALLVARIABLE          { TokenSmallVariable $$ }
      AND                    { TokenAnd }
      OR                     { TokenOr }
      IMPLICATION            { TokenImplication }
      NOT                    { TokenNot }
      OPENP                  { TokenOP }
      CLOSEP                 { TokenCP }
      MULT                   { TokenMultiplication }
      PLUS                   { TokenSum }
      COLON                  { TokenColon }
      DOT                    { TokenDot }
      ZERO                   { TokenZero }
      EXISTS                 { TokenExists }
      FORALL                 { TokenForall }
      EQUALS                 { TokenEquals }
      COMA                   { TokenComa }
%%

Expression  : Disjunction                        { $1 }
            | Disjunction IMPLICATION Expression { BinaryOperator Implication $1 $3 }

Disjunction : Conjunction                        { $1}
            | Disjunction OR Conjunction         { BinaryOperator Or $1 $3 }

Conjunction : Unary                              { $1 }
            | Conjunction AND Unary              { BinaryOperator And $1 $3 }

Unary       : Predicate                          { $1 }
            | NOT Unary                          { Not $2 }
            | OPENP Expression CLOSEP            { $2 }
            | EXISTS Variable DOT Expression     { Quantifiers Exists $2 $4 }
            | FORALL Variable DOT Expression     { Quantifiers Forall $2 $4 }

Variable    : SMALLVARIABLE                      { $1 }

Predicate   : CAPVARIABLE OPENP List CLOSEP      { Predication (Predicate $1 $3) }
            | Therm EQUALS Therm                 { Predication (Equals $1 $3) }

Therm       : Addend                             { $1 }
            | Therm PLUS Addend                  { Application (Plus $1 $3) }

Addend      : Mult                               { $1 }
            | Addend MULT Mult                   { Application (Multiplication $1 $3) }

Mult        : Variable OPENP List CLOSEP         { Application (Function $1 $3) }
            | Variable                           { Variable $1 }
            | OPENP Therm CLOSEP                 { $2 }
            | ZERO                               { Application Zero }
            | Mult COLON                         { Application (Colon $1) }

List        : Therm COMA List                    { $1:$3 }
            | Therm                              { [$1] }
{
parseError :: [Token] -> a
parseError e = error $ "Parse error " ++ (show e)
}
