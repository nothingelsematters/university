module Logic.Proof where

import Logic.Expression
import Logic.Parsing.ExpressionParser
import Data.List (intercalate)
import Data.HashMap.Strict as HMap (fromList, singleton, foldlWithKey', lookup, insert, (!))

data Proof = Proof { hypotheses  :: [Expression]
                   , conclusion  ::  Expression
                   , expressions :: [Expression] }

instance Show Proof where
    show p = showHypotheses ++ "|- " ++ (show . conclusion $ p) ++ "\n" ++ intershow "\n" expressions
        where intershow str meth = (intercalate str) . (map show) . meth $ p
              showHypotheses | null $ hypotheses p = ""
                             | otherwise           = intershow ", " hypotheses ++ " "


infix 6 ~> -- maps template
(~>) :: [(String, Expression)] -> [String] -> [Expression]
exprs ~> tmpl = map (mapExpression . getExpression) tmpl
    where
        expsMap = HMap.fromList exprs
        mapExpression (BinaryOperator op l r) = BinaryOperator op (mapExpression l) (mapExpression r)
        mapExpression (Variable name)         = expsMap HMap.! name
        mapExpression (Not ex)                = Not (mapExpression ex)

axioms :: [Expression]
axioms = map getExpression
    [
        "A -> (B -> A)",
        "(A -> B) -> (A -> B -> G) -> (A -> G)",
        "A -> B -> A & B",
        "A & B -> A",
        "A & B -> B",
        "A -> A | B",
        "B -> A | B",
        "(A -> G) -> (B -> G) -> (A | B -> G)",
        "(A -> B) -> (A -> !B) -> !A",
        "!!A -> A"
    ]

isAxiom :: Expression -> Bool
isAxiom expr = any (expr @?) axioms

infix 4 @? -- does expression fit axiom
(@?) :: Expression -> Expression -> Bool
expr @? pattern = compHelper expr pattern /= Nothing
    where compHelper (BinaryOperator op1 exl exr) (BinaryOperator op2 patl patr) =
              if op1 /= op2
                  then Nothing
                  else compAssoc (compHelper exl patl) (compHelper exr patr)
          compHelper (Not expr') (Not pattern') = compHelper expr' pattern'
          compHelper expr' (Variable s) = Just (HMap.singleton s expr')
          compHelper _ _ = Nothing

          compAssoc _ Nothing = Nothing
          compAssoc Nothing _ = Nothing
          compAssoc left (Just right) = HMap.foldlWithKey' assocFolder left right

          assocFolder Nothing _ _ = Nothing
          assocFolder (Just was) f s
              | HMap.lookup f was == Nothing = Just (HMap.insert f s was)
              | HMap.lookup f was == Just s  = Just was
              | otherwise                   = Nothing
