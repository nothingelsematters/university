module Logic.CompleteProof (completeProof) where

import Logic.Expression
import Logic.Proof
import Logic.Proofs

import Data.Maybe          ()
import Data.Bits           (testBit, shiftR, (.|.), (.&.))
import Data.List           (minimumBy)
import Data.Ord            (comparing)
import Control.Applicative ((<$>))


completeProof :: Expression -> Maybe Proof
completeProof expr = generateProof expr <$> minimumRequirements expr

generateProof :: Expression -> ([String], Bool) -> Proof
generateProof expr (variables, value) = makeProof
    (if value then expr else Not expr)
    (zip variables (repeat value))

minimumRequirements :: Expression -> Maybe ([String], Bool)
minimumRequirements expr
    | check True  = makeValues True
    | check False = makeValues False
    | otherwise   = Nothing

    where
        (vars, vals) = getValues expr
        check        = not . null . getAccepted
        makeValues value = Just $ (giveValues $ minimumBy (comparing bitsAmount) $ getAccepted value, value)

        bitsAmount 0    = 0
        bitsAmount mask = (mask .&. 1) + shiftR mask 1
        giveValues mask = give mask vars
            where
                give _ []    = []
                give m (h:t)
                    | testBit m 0 = h : give (shiftR m 1) t
                    | otherwise   = give (shiftR m 1) t

        getAccepted :: Bool -> [Int]
        getAccepted value = foldr varSetIteration [] [0..(length vals - 1)]
            where
                varSetIteration mask was
                    | foldr valIteration True (zip [0..] vals) = mask : was
                    | otherwise                                = was
                    where
                        valIteration _ False      = False
                        valIteration (mask', val) was'
                            | consider mask mask' = val == value
                            | otherwise           = was'

                        consider was' new
                            | value     = was' .|. new == new
                            | otherwise = was' .|. new == new + was'
