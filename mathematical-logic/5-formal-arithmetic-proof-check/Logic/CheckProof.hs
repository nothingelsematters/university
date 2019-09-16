module Logic.CheckProof (processLine) where

import qualified Data.Set        as Set (member)
import qualified Data.Map.Strict as Map ((!?), member, insert, insertWith)

import Logic.Expression
import Logic.Proof
import Logic.CheckAxiom


processLine :: Proof -> Expression -> Either String Proof
processLine proof expr
    | correct   = Right . add $ expr
    | otherwise = Left $ "Line #" ++ show (size proof + 1) ++ " can't be obtained"
    where
        -- check zone --
        correct = any ($ expr) [isHypothesis, isAxiom, isModusPonens, isDerivedExists, isDerivedForall]

        isHypothesis  expression = expression `Set.member` hypotheses proof
        isModusPonens expression = (Just True ==) $ any (flip Map.member (indexes proof) . fst) <$> modusPonens proof Map.!? expression

        isDerivedExists (BinaryOperator Implication (Quantifiers Exists v l) r) = checkQuantifierDerived v r l r
        isDerivedExists _                                                       = False

        isDerivedForall (BinaryOperator Implication l (Quantifiers Forall v r)) = checkQuantifierDerived v l l r
        isDerivedForall _                                                       = False

        checkQuantifierDerived variable checkPart l r = checkClosed variable checkPart && BinaryOperator Implication l r `Map.member` indexes proof

        -- update zone --
        add expression@(BinaryOperator Implication l r) =
            (add' expression) { modusPonens = Map.insertWith (flip (++)) r [(l, size proof + 1)] $ modusPonens proof }
        add expression = add' expression

        add' expression =
            proof { obtained = obtained proof || expression == conclusion proof
                  , indexes  = Map.insert expression (size proof + 1) $ indexes proof
                  , size     = size proof + 1 }
