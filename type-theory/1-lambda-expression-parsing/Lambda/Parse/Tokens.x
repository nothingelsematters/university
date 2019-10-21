{
module Lambda.Parse.Tokens where
}

%wrapper "basic"

$smallletters = [a-z]
$small = [0-9 $smallletters \']

tokens :-
  $white+                       ;
  \(                            { \_ -> TokenOP }
  \)                            { \_ -> TokenCP }
  $smallletters[$small]*        { \x -> TokenVariable x }
  \.                            { \_ -> TokenDot }
  \\                            { \_ -> TokenLambda }

{
data Token
      = TokenOP
      | TokenCP
      | TokenLambda
      | TokenDot
      | TokenVariable String
      deriving Show
}
