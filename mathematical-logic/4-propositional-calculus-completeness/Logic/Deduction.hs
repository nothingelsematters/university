module Logic.Deduction where

import Logic.Expression
import Logic.Proof
import Data.Maybe (isJust, fromJust)
import Data.List (foldl')
import qualified Data.HashSet        as HSet (empty, member, insert, fromList)
import qualified Data.HashMap.Strict as HMap (empty, member, insert, (!))

deduce :: Proof -> Proof
deduce (Proof hyps res exps) = Proof (init hyps) (BinaryOperator Implication adding res) (fst' content)
    where
        fst' (a, _, _) = a
        adding         = last hyps
        hypsSet        = HSet.fromList hyps
        content        = foldl' func ([], HMap.empty, HSet.empty) exps
        func (ready, mp, expSet) expr = (updateProof, updateMP expr, expr `HSet.insert` expSet)
            where
                updateProof
                    | adding == expr = ready ++ expressions (proofAToA expr)
                    | isAxiom expr || expr `HSet.member` hypsSet = ready ++ [("E", expr), ("A", adding)] ~>
                        [
                            "E",
                            "E -> A -> E",
                            "A -> E"
                        ]
                    | otherwise      = ready ++ [("A", adding), ("D", expr), ("F", fromJust findMP)] ~>
                        [
                            "(A -> F) -> (A -> (F -> D)) -> A -> D",
                            "(A -> F -> D) -> A -> D",
                            "A -> D"
                        ]

                updateMP (BinaryOperator Implication l r)
                    | r `HMap.member` mp = HMap.insert r (l : (mp HMap.! r)) mp
                    | otherwise          = HMap.insert r [l] mp
                updateMP _ = mp

                findMP = foldr finder Nothing (mp HMap.! expr)
                finder ex ret
                    | isJust ret              = ret
                    | ex `HSet.member` expSet = Just ex
                    | otherwise               = Nothing


proofAToA :: Expression -> Proof
proofAToA expr = Proof [] (expr *->* expr) $ [("A", expr)] ~>
    [
        "A -> A -> A",
        "(A -> A -> A) -> (A -> (A -> A) -> A) -> A -> A",
        "(A -> (A -> A) -> A) -> A -> A",
        "A -> (A -> A) -> A",
        "A -> A"
    ]
