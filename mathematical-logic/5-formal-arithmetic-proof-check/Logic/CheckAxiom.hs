module Logic.CheckAxiom (isAxiom) where

import Control.Monad     (join)
import Control.Exception (assert)
import Data.Function     (on)
import Data.Char         (isAlpha)
import Data.Maybe        (isJust, isNothing, fromJust)

import qualified Data.Map.Strict as Map (Map, empty, singleton, insert, (!?), notMember, foldlWithKey', toList)
import qualified Data.Set        as Set (empty, singleton, insert, union, disjoint, member)

import Logic.Parsing.ExpressionParser
import Logic.Expression


isAxiom :: Expression -> Bool
isAxiom expr = any (\f -> f expr) [isPropositionalAxiom, isPredicateAxiom, isFormalArithmeticAxiom]

-- common zone --
commonFits :: (Ord k, Eq v) => (Expression -> Expression -> Maybe (Map.Map k v)) -> Expression -> Expression -> Maybe (Map.Map k v)
commonFits fun (BinaryOperator op1 exl exr) (BinaryOperator op2 patl patr)
    | op1 == op2 = on compareVariables (uncurry fun) (exl, patl) (exr, patr)
    | otherwise  = Nothing
commonFits fun (Not expr') (Not pattern') = fun expr' pattern'
commonFits _ _ _                          = Nothing

compareVariables :: (Ord k, Eq v) => Maybe (Map.Map k v) -> Maybe (Map.Map k v) -> Maybe (Map.Map k v)
compareVariables left right = join $ Map.foldlWithKey' compareFolder left <$> right
    where
        compareFolder Nothing _ _ = Nothing
        compareFolder m@(Just was) f s
            | Map.notMember f was    = Just $ Map.insert f s was
            | was Map.!? f == Just s = m
            | otherwise              = Nothing

commonBelongs :: (Ord k, Eq v) => (Expression -> Expression -> Maybe (Map.Map k v)) -> [Expression] -> Expression -> Bool
commonBelongs fits list expr = any (\x -> isJust $ fits expr x) list

isSubstitution :: String -> Expression -> Expression -> Bool
isSubstitution x l r = isJust $ whatSubstitutes x l r

whatSubstitutes :: String -> Expression -> Expression -> Maybe Therm
whatSubstitutes x l r = transform <$> fits Set.empty l r
    where
        transform m = snd . head . (\z -> assert (length z == 1) z) . Map.toList $ m

        -- fit zone --
        fits closed (Predication expr') (Predication pattern') = fitsPSymbol closed expr' pattern'
        fits closed (Quantifiers q v e) (Quantifiers q' v' e')
            | q /= q' || v /= v' = Nothing
            | v == x             = Just Map.empty
            | otherwise          = fits (v `Set.insert` closed) e e'
        fits closed expr' pattern'                             = commonFits (fits closed) expr' pattern'

        fitsPSymbol closed (Equals a b) (Equals a' b') = compareTwoTherms closed (a, a') (b, b')
        fitsPSymbol closed (Predicate s th) (Predicate s' th')
            | s /= s'   = Nothing
            | otherwise = checkTwoLists closed th th'
        fitsPSymbol  _ _ _ = Nothing

        fitsTherm closed (Variable s) th
            | s == x && checkFreeSubstitution closed th = Just $ Map.singleton s th
            | s == x                                    = Nothing
            | otherwise                                 = Just Map.empty
        fitsTherm closed (Application a) (Application a') = fitsFSymbol closed a a'
        fitsTherm _ _ _                                   = Nothing

        fitsFSymbol closed (Plus a b) (Plus a' b')                     = compareTwoTherms closed (a, a') (b, b')
        fitsFSymbol closed (Multiplication a b) (Multiplication a' b') = compareTwoTherms closed (a, a') (b, b')
        fitsFSymbol closed (Colon th) (Colon th')                      = fitsTherm closed th th'
        fitsFSymbol _ Zero Zero                                        = Just Map.empty
        fitsFSymbol closed (Function f th) (Function f' th')
            | f /= f'   = Nothing
            | otherwise = checkTwoLists closed th th'
        fitsFSymbol _ _ _                                              = Nothing

        checkTwoLists closed f s
            | on (/=) length f s = Nothing
            | otherwise          = join . (check <$>) . foldr func (Just []) $ zip f s
            where
                func (a, b) was
                    | isNothing was                                      = Nothing
                    | a == Variable x && checkFreeSubstitution closed b  = Just $ b : fromJust was
                    | a == Variable x                                    = Nothing
                    | otherwise                                          = was

                check list
                    | null list               = Just Map.empty
                    | all (head list ==) list = Just $ x `Map.singleton` head list
                    | otherwise               = Nothing

        compareTwoTherms closed      = on compareVariables (uncurry $ fitsTherm closed)
        checkFreeSubstitution closed = Set.disjoint closed . getOpened Set.empty

        -- get open variables in therm zone --
        getOpened closed (Application f) = getOpenedF closed f
        getOpened closed (Variable v)
            | v `Set.member` closed = Set.empty
            | otherwise          = Set.singleton v

        getOpenedF closed (Plus a b)           = fromTwoTherms closed a b
        getOpenedF closed (Multiplication a b) = fromTwoTherms closed a b
        getOpenedF closed (Colon th)           = getOpened closed th
        getOpenedF closed (Function _ th)      = foldr Set.union Set.empty $ map (getOpened closed) th
        getOpenedF _  Zero                     = Set.empty

        fromTwoTherms closed     = on (Set.union) $ getOpened closed

-- proposition calculus axiom schemes zone --
propositionalAxiomSchemes :: [Expression]
propositionalAxiomSchemes = map (getExpression . foldr process [])
    [ "a -> (b -> a)"
    , "(a -> b) -> (a -> b -> g) -> (a -> g)"
    , "a -> b -> a & b"
    , "a & b -> a"
    , "a & b -> b"
    , "a -> a | b"
    , "b -> a | b"
    , "(a -> g) -> (b -> g) -> (a | b -> g)"
    , "(a -> b) -> (a -> !b) -> !a"
    , "!!a -> a"
    ]
    where
        process char was
            | isAlpha char = ['(', char, '=', char, ')'] ++ was
            | otherwise    = char : was

isPropositionalAxiom :: Expression -> Bool
isPropositionalAxiom = commonBelongs fits propositionalAxiomSchemes
    where
        fits expr' (Predication (Equals (Variable s) _)) = Just $ Map.singleton s expr'
        fits expr' pattern'                              = commonFits fits expr' pattern'

-- predicate calculus axiom schemes zone --
isPredicateAxiom :: Expression -> Bool
isPredicateAxiom (BinaryOperator Implication (Quantifiers Forall x psi) psi') = isSubstitution x psi psi'
isPredicateAxiom (BinaryOperator Implication psi' (Quantifiers Exists x psi)) = isSubstitution x psi psi'
isPredicateAxiom _                                                            = False

-- formal arithmetic axioms zone --
formalArithmeticAxioms :: [Expression]
formalArithmeticAxioms = map getExpression
    [ "a = b -> a' = b'"
    , "a = b -> a = c -> b = c"
    , "a' = b' -> a = b"
    , "!a' = 0"
    , "a + b' = (a + b)'"
    , "a + 0 = a"
    , "a * 0 = 0"
    , "a * b' = a * b + a"
    ]

isFormalArithmeticAxiom :: Expression -> Bool
isFormalArithmeticAxiom expr = commonBelongs fits formalArithmeticAxioms expr || isFormalArithmeticAxiomScheme expr
    where
        fits (Predication expr') (Predication pattern') = fitsPSymbol expr' pattern'
        fits expr' pattern'                             = commonFits fits expr' pattern'

        fitsPSymbol (Equals a b) (Equals a' b') = fitsTwoTherms (a, a') (b, b')
        fitsPSymbol  _ _                        = Nothing

        fitsTherm (Variable t) (Variable s)        = Just $ Map.singleton s t
        fitsTherm (Application f) (Application f') = fitsFSymbol f f'
        fitsTherm _ _                              = Nothing

        fitsFSymbol (Plus a b) (Plus a' b')                     = fitsTwoTherms (a, a') (b, b')
        fitsFSymbol (Multiplication a b) (Multiplication a' b') = fitsTwoTherms (a, a') (b, b')
        fitsFSymbol (Colon c) (Colon c')                        = fitsTherm c c'
        fitsFSymbol Zero Zero                                   = Just Map.empty
        fitsFSymbol _ _                                         = Nothing

        fitsTwoTherms = on compareVariables (uncurry fitsTherm)

        -- axiom scheme: (phi[x := 0]) & Forall x (phi -> phi[x := x']) -> phi --
        isFormalArithmeticAxiomScheme (BinaryOperator Implication (BinaryOperator And begin (Quantifiers Forall x (BinaryOperator Implication l r))) end) =
            l == end && whatSubstitutes x l r == Just (Application . Colon . Variable $ x) && whatSubstitutes x l begin == Just (Application Zero)
        isFormalArithmeticAxiomScheme _ = False
