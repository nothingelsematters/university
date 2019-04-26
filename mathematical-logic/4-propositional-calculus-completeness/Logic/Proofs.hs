module Logic.Proofs
    ( makeProof
    , excludeMiddle      -- a | !a
    , contraposition     -- (a -> b) -> (!b -> !a)
    , doubleNegation     -- a -> !!a
    , admissionExclusion -- ..., r |- a  || ..., !r |- a ==> ... |- a
    ) where

import Logic.Expression
import Logic.Proof
import Logic.Deduction

import Data.List     ((\\))
import Data.Function (on)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map


makeProof :: Expression -> [(String, Bool)] -> Proof
makeProof expr fixedVars = Proof hyps expr content
    where
        hyps     = map (\(s, v) -> if v then Variable s else Not (Variable s)) fixedVars
        content  = expressions $ curry removeAllAdmissions freeVars $ proofList expr fixedVars freeVars
        freeVars = (variablesList expr) \\ map fst fixedVars

proofList :: Expression -> [(String, Bool)] -> [String] -> [Proof]
proofList expr fixedVars freeVars = map (\m -> snd $ proofEvaluated expr m) values'
    where
        values' = map (\x -> on Map.union Map.fromList fixedVars (zip freeVars x)) $
            replicateM (length freeVars) [False, True]

removeAllAdmissions :: ([String], [Proof]) -> Proof
removeAllAdmissions z@(freeVars, proofs) = if null freeVars
    then head proofs
    else removeAllAdmissions $ removeAdmission z

removeAdmission :: ([String], [Proof]) -> ([String], [Proof])
removeAdmission (freeVars, proofs) = (tail freeVars, proofs')
    where
        (left, right) = splitAt ((length proofs) `div` 2) proofs
        adding  = head freeVars
        left'   = mapPart left (Not . Variable)
        right'  = mapPart right (Variable)
        pairs   = zip (zip left' right') left
        proofs' = map (\(x,_) -> admissionExclusion (Variable adding) x) pairs

        mapPart part func = map (\x -> deduce $ x { hypotheses = (hypotheses x ++) $ reverse . map func $ freeVars } ) part


valuesMapper :: (String, Bool) -> Expression
valuesMapper (str, value) = if value then Variable str else Not (Variable str)

hypsFromMap :: Map.Map String Bool -> [Expression]
hypsFromMap = (map valuesMapper) . Map.toList

contraposition :: Expression -> Proof
contraposition expr@(BinaryOperator Implication l r) =
    iterate deduce (Proof [expr, Not r] (Not l) ([("A", l), ("B", r)] ~> tmpl)) !! 2
    where
        tmpl =
            [
                "(A -> B) -> (A -> !B) -> !A",
                "A -> B",
                "(A -> !B) -> !A",
                "!B -> (A -> !B)",
                "!B",
                "A -> !B",
                "!A"
            ]
contraposition _ = undefined

excludeMiddle :: Expression -> Proof
excludeMiddle expr = Proof [] (expr *|* Not expr) $
    list ~> ["A -> A | !A"] ++ expressions (contraposition (expr *->* expr *|* Not expr)) ++
    list ~> ["!(A | !A) -> !A", "!A -> A | !A"] ++ expressions (contraposition (Not expr *->* expr *|* Not expr)) ++
    list ~>
    [
        "!(A | !A) -> !!A",
        "(!(A | !A) -> !A) -> (!(A | !A) -> !!A) -> (!!(A | !A))",
        "(!(A | !A) -> !!A) -> !!(A | !A)",
        "!!(A | !A)",
        "!!(A | !A) -> (A | !A)",
        "A | !A"
    ]
    where
        list = [("A", expr)]

admissionExclusion :: Expression -> (Proof, Proof) -> Proof
admissionExclusion what (left@(Proof hyps (BinaryOperator Implication _ res) _), right) = Proof hyps res $
    on (++) expressions left right ++
    expressions (excludeMiddle what) ++
    [("A", res), ("R", what)] ~>
    [
        "(R -> A) -> (!R -> A) -> (R | !R) -> A",
        "(!R -> A) -> (R | !R -> A)",
        "R | !R -> A",
        "A"
    ]
admissionExclusion _ _ = undefined

doubleNegation :: Expression -> Proof
doubleNegation expr = Proof [] (Not (Not expr)) $ expressions (proofAToA (Not expr)) ++ [("A", expr)] ~>
    [
        "A",
        "A -> !A -> A",
        "!A -> A",
        "(!A -> A) -> (!A -> !A) -> !!A",
        "(!A -> !A) -> !!A",
        "!!A"
    ]

proofEvaluated :: Expression -> Map.Map String Bool -> (Bool, Proof)
proofEvaluated expr@(BinaryOperator And l r) values
    | lProven && rProven = proofCommonPart True $ on (++) expressions lProof rProof ++ varsList ~>
        [
            "A -> B -> A & B",
            "B -> A & B",
            "A & B"
        ]
    | not lProven        = falseResult lProof
    | otherwise          = falseResult rProof
    where
        (lProven, lProof) = proofEvaluated l values
        (rProven, rProof) = proofEvaluated r values
        varsList          = [("A", l), ("B", r)]
        proofCommonPart value exprs = (,) value $ Proof (hypsFromMap values) expr exprs
        falseResult proof = proofCommonPart False $ expressions proof ++ (("C", conclusion proof) : varsList) ~>
            [
                "(A & B -> C) -> (A & B -> !C) -> !(A & B)",
                "A & B -> C",
                "(A & B -> !C) -> !(A & B)",
                "!C -> A & B -> !C",
                "A & B -> !C",
                "!(A & B)"
            ]

proofEvaluated expr@(BinaryOperator Or l r) values
    | lProven   = trueResult lProof
    | rProven   = trueResult rProof
    | otherwise = proofCommonPart False $ (concat . (map expressions)) [lProof, rProof, proofAToA l, snd (proofEvaluated (r *->* l) values)] ++ varsList ~>
        [
            "(A | B -> A) -> (A | B -> !A) -> !(A | B)",
            "(A -> A) -> (B -> A) -> (A | B -> A)",
            "(B -> A) -> (A | B -> A)",
            "(A | B -> A)",
            "(A | B -> !A) -> !(A | B)",
            "!A -> A | B -> !A",
            "A | B -> !A",
            "!(A | B)"
        ]
    where
        (lProven, lProof) = proofEvaluated l values
        (rProven, rProof) = proofEvaluated r values
        varsList          = [("A", l), ("B", r)]
        proofCommonPart value exprs = (,) value $ Proof (hypsFromMap values) expr exprs
        trueResult proof  = proofCommonPart True $ expressions proof ++ (("C", conclusion proof) : varsList) ~>
            [
                "C -> (A | B)",
                "A | B"
            ]

proofEvaluated expr@(BinaryOperator Implication l r) values
    | rProven                = proofCommonPart True  $ expressions rProof ++ varsList ~> ["B -> A -> B", "A -> B"]
    | lProven && not rProven = proofCommonPart False $ on (++) expressions lProof rProof ++
        varsList ~> ["((A -> B) -> B) -> ((A -> B) -> !B) -> !(A -> B)"] ++
        expressions (deduce $ Proof [l, Not r, l *->* r] r [l, l *->* r, r]) ++
        varsList ~>
            [
                "((A -> B) -> !B) -> !(A -> B)",
                "!B -> (A -> B) -> !B",
                "(A -> B) -> !B",
                "!(A -> B)"
            ]
    | otherwise  = proofCommonPart True $ expressions (contraposition (Not r *->* Not l)) ++
        (expressions . snd) (proofEvaluated (Not r *->* Not l) values) ++
        varsList ~> ["!!A -> !!B"] ++
        expressions (deduce $ Proof [Not (Not l) *->* Not (Not r), l] r $ expressions (doubleNegation l) ++ varsList ~>
            [
                "!!A -> !!B",
                "!!B",
                "!!B -> B",
                "B"
            ]) ++
        varsList ~> ["A -> B"]
    where
        (lProven, lProof) = proofEvaluated l values
        (rProven, rProof) = proofEvaluated r values
        varsList          = [("A", l), ("B", r)]
        proofCommonPart value exprs = (,) value $ Proof (hypsFromMap values) expr exprs

proofEvaluated expr@(Not ex) values
    | eProven   = proofCommonPart False $ expressions eProof ++ expressions (doubleNegation ex)
    | otherwise = proofCommonPart True  $ expressions eProof
    where
        (eProven, eProof) = proofEvaluated ex values
        proofCommonPart value exprs = (,) value $ Proof (hypsFromMap values) expr exprs

proofEvaluated expr@(Variable str) values
    | values Map.! str = proofCommonPart True  [expr]
    | otherwise         = proofCommonPart False [Not expr]
    where
        proofCommonPart value exprs = (,) value $ Proof (hypsFromMap values) expr exprs
