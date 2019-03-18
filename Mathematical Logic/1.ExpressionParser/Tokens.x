{
module Tokens where
}

%wrapper "basic"

$alpha = [A-Z]
$variableSymbols = [$alpha 0-9 \']
$spaces = [\ \t]

tokens :-
  $white+                       ;
  \(                            { \x -> TokenOP }
  \)                            { \x -> TokenCP }
  \|                            { \x -> TokenOr }
  &                             { \x -> TokenAnd }
  !                             { \x -> TokenNot }
  "->"                          { \x -> TokenImplication }
  $alpha[$variableSymbols]*     { \x -> TokenVariable x }

{
data Token
      = TokenVariable String
      | TokenAnd
      | TokenOr
      | TokenNot
      | TokenImplication
      | TokenOP
      | TokenCP
      deriving Show
}
