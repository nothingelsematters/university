module ProofMinimizator where

import Data.Maybe
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
    (Map, insert, empty, singleton, foldlWithKey', member, lookup, toList, fromList, update)
import qualified Data.IntMap.Strict as IntMap (insert, singleton, (!), member, notMember)

import TaskGrammar
import ExpressionGrammar
import Tokens
import Proof

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

getTask :: String -> Task
getTask = parseTask . alexScanTokens

getExpression :: String -> Expression
getExpression = parseExpression . alexScanTokens

fromTask :: Task -> HalfProof
fromTask task = HalfProof (Map.fromList makePairs) (result $ task) Map.empty Map.empty 1 False
    where makePairs = fst $ foldl' (\(a, n) h -> ((h, n):a, n + 1)) ([], 1) (hypotheses task)

addExpression :: Expression -> HalfProof -> Maybe HalfProof
addExpression ex proof =
    if Map.member ex (exps proof)
        then checked
        else case factory of
            Nothing   -> Nothing
            Just made -> Just $ proof {
              exps    = Map.insert ex (made, size proof) (exps proof),
              size    = (size proof) + 1,
              parts   = updateParts ex,
              failure = False }

    where factory | isAx      = Just $ Axiom indexA
                  | isHyp     = Just $ Hypothesis indexH
                  | isMP      = Just $ uncurry ModusPonens (fromJust indexesMP)
                  | otherwise = Nothing
          check = (/= -1)
          checked = if ex == res proof
              then Just $ proof { exps    = Map.update (\(t, _) -> Just (t, size proof)) ex (exps proof),
                                  size    = (size proof) + 1,
                                  failure = False }
              else Just $ proof { failure = True }

          -- check for hypothesis --
          isHyp = check indexH
          indexH = case Map.lookup ex (hyp proof) of
                    Nothing   -> -1
                    Just smth -> smth

          -- check for modus ponens --
          isMP = isJust indexesMP
          indexesMP :: Maybe (Int, Int)
          indexesMP = case Map.lookup ex $ parts proof of
              Nothing        -> Nothing
              Just indexlist -> foldr folder Nothing indexlist
              where folder _ (Just smth) = Just smth
                    folder (expr, num) _ =
                        case Map.lookup expr (exps proof) of
                            Nothing  -> Nothing
                            Just ind -> Just (num, snd ind)

          updateParts (BinaryOperator Implication a b) =
              if Map.member b $ parts proof
                  then Map.update (\x -> Just $ (a, size proof):x) b $ parts proof
                  else Map.insert b [(a, size proof)] $ parts proof
          updateParts _ = parts proof

          -- check for axiom --
          isAx = check indexA
          indexA = helper 1 axioms
              where helper _ [] = -1
                    helper pos (h:t) = if ex `comp` h then pos else helper (pos + 1) t
          comp expr pattern = compHelper expr pattern /= Nothing
              where compHelper (BinaryOperator op1 exl exr) (BinaryOperator op2 patl patr) =
                        if op1 /= op2
                            then Nothing
                            else compAssoc (compHelper exl patl) (compHelper exr patr)
                    compHelper (Not expr) (Not pattern) = compHelper expr pattern
                    compHelper expr (Variable s) = Just (Map.singleton s expr)
                    compHelper _ _ = Nothing

                    compAssoc _ Nothing = Nothing
                    compAssoc Nothing _ = Nothing
                    compAssoc left (Just right) = Map.foldlWithKey' assocFolder left right

                    assocFolder Nothing _ _ = Nothing
                    assocFolder (Just was) f s
                        | Map.lookup f was == Nothing = Just (Map.insert f s was)
                        | Map.lookup f was == Just s = Just was
                        | otherwise = Nothing


sortProof :: HalfProof -> SortedProof
sortProof proof =
    SortedProof (Task ((map fst) . (sortOn snd) . Map.toList . hyp $ proof) (res proof))
                ((sortOn (snd . snd)) . Map.toList . exps $ proof)

minimizeProof :: SortedProof -> Maybe SortedProof
minimizeProof sproof =
    if null (expressions sproof) || (fst . last $ (expressions sproof)) /= (result . task $ sproof)
        then Nothing
        else Just (sproof { expressions = handleUseless })

    where handleUseless = firster $ foldl' folder ([], usable, 1, 1) (expressions sproof)
          firster (x, _, _, _) = x
          folder (done, mapp, num, cur) ex =
              if IntMap.notMember cur mapp
                  then (done, mapp, num, cur + 1)
                  else (process ex done mapp num, IntMap.insert cur num mapp, num + 1, cur + 1)
          process (ex, (ModusPonens x y, _)) done mapp num =
              (ex, (ModusPonens (mapp IntMap.! x) (mapp IntMap.! y), 0)) : done
          process el done mapp _ = el : done

          l = length . expressions $ sproof
          usable = fst $ foldr folder' (IntMap.singleton l 0, l)  (expressions sproof)
          folder' (_, (ModusPonens x y, _)) (was, num) = (mapConstruct num was x y, num - 1)
          folder' _ (was, num) = (was, num - 1)
          mapConstruct num mapp x y =
              if IntMap.member num mapp
                  then (IntMap.insert x 0) . (IntMap.insert y 0) $ mapp
                  else mapp
