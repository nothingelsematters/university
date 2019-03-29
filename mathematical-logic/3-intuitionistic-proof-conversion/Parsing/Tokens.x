{
module Parsing.Tokens where
}

%wrapper "basic"

$alpha = [A-Z]
$variableSymbols = [$alpha 0-9 \']
$spaces = [\ \t \r]

tokens :-
  $spaces+                      ;
  \(                            { \x -> TokenOP }
  \)                            { \x -> TokenCP }
  \|                            { \x -> TokenOr }
  &                             { \x -> TokenAnd }
  !                             { \x -> TokenNot }
  "->"                          { \x -> TokenImplication }
  "|-"                          { \x -> TokenTourniquet }
  \,                            { \x -> TokenComa }
  \n                            { \x -> TokenNewLine }
  $alpha[$variableSymbols]*     { \x -> TokenVariable x }

{
data Token
      = TokenVariable String
      | TokenAnd
      | TokenOr
      | TokenNot
      | TokenImplication
      | TokenTourniquet
      | TokenOP
      | TokenCP
      | TokenComa
      | TokenNewLine
      deriving Show
}
