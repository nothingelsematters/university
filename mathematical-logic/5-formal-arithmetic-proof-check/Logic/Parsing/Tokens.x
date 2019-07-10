{
module Logic.Parsing.Tokens where
}

%wrapper "basic"

$digits = [0-9]
$smallletters = [a-z]
$capitalletters = [A-Z]
$small = [$digits $smallletters]
$capital = [$digits $capitalletters]
$spaces = [\ \t \r]

tokens :-
  $spaces+                      ;
  \(                            { \_ -> TokenOP }
  \)                            { \_ -> TokenCP }
  \|                            { \_ -> TokenOr }
  &                             { \_ -> TokenAnd }
  !                             { \_ -> TokenNot }
  "->"                          { \_ -> TokenImplication }
  "|-"                          { \_ -> TokenTourniquet }
  \,                            { \_ -> TokenComa }
  \n                            { \_ -> TokenNewLine }
  $smallletters[$small]*        { \x -> TokenSmallVariable x }
  $capitalletters[$capital]*    { \x -> TokenCapVariable x }
  \*                            { \_ -> TokenMultiplication }
  \+                            { \_ -> TokenSum }
  @                             { \_ -> TokenForall }
  \?                            { \_ -> TokenExists }
  0                             { \_ -> TokenZero }
  '                             { \_ -> TokenColon }
  \.                            { \_ -> TokenDot }
  =                             { \_ -> TokenEquals }

{
data Token
      = TokenCapVariable String
      | TokenSmallVariable String
      | TokenAnd
      | TokenOr
      | TokenNot
      | TokenImplication
      | TokenTourniquet
      | TokenOP
      | TokenCP
      | TokenComa
      | TokenNewLine
      | TokenMultiplication
      | TokenSum
      | TokenForall
      | TokenExists
      | TokenZero
      | TokenColon
      | TokenDot
      | TokenEquals
      deriving Show
}
