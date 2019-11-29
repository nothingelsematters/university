module Lambda.TypeDeduction where

import Control.Monad (join)
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map (Map, empty, insert, member, (!), delete, map, foldlWithKey', foldrWithKey', size)
import Lambda.Expression

data Type = AtomicType   { value :: String }
          | FunctionType { leftt :: Type, rightt :: Type }
          deriving (Eq)

instance Show Type where
    show (AtomicType s) = s
    show (FunctionType l r) = concat ["(", show l, " -> ", show r, ")"]


data Rule = VariableRule    { expression :: Expression }
          | LambdaRule      { expression :: Expression, index  :: String, proof  :: Rule }
          | ApplicationRule { expression :: Expression, proofl :: Rule,   proofr :: Rule }

data TypeProof = TypeProof { rule :: Rule, types :: Map.Map String Type }

infix 2 #
(#) :: Rule -> Map.Map String Type -> TypeProof
(#) = TypeProof

getType :: TypeProof -> Type
getType (TypeProof (VariableRule    e     ) h) = h Map.! name e
getType (TypeProof (ApplicationRule _ l  _) h) = rightt $ getType (l # h)
getType (TypeProof (LambdaRule      _ lt _) h) = h Map.! lt

instance Show TypeProof where
    show = showIndented ""
        where
            showIndented indent x@(TypeProof (VariableRule    e     ) h) = showRule indent h e x 1
            showIndented indent x@(TypeProof (ApplicationRule e l  r) h) =
                showRule indent h e x 2 ++ on (++) (showIndented' indent $ h) l r

            showIndented indent x@(TypeProof (LambdaRule      e lt d) h) =
                showRule indent h e x 3 ++ (showIndented' indent $ Map.insert (variable e) (leftt $ h Map.! lt) h) d

            showIndented' indent h rl = showIndented (indent ++ "*   ") . flip (#) h $ rl

            showRule :: String -> Map.Map String Type -> Expression -> TypeProof -> Int -> String
            showRule ind hyps expr typp num =
                concat [ind, stringHyps, tourniquet, showTypes expr (getType typp), " [rule #", show num, "]\n"]
                where
                    stringHyps            = showHypotheses hyps
                    tourniquet            = (if null $ stringHyps then "" else " ") ++ "|- "
                    showHypotheses        = intercalate ", " . filter (not . null) . Map.foldlWithKey' folder []
                    folder was key val    = (if head key == '\\' then "" else concat [key, " : ", show val]) : was
                    showTypes expr' typp' = concat [show expr', " : ", show typp']


deduceType :: Expression -> Maybe TypeProof
deduceType expr = snd <$> deducer Map.empty 1 expr
    where
        newType n = AtomicType $ "t" ++ show n
        newLambda n = "\\" ++ show n
        updateMap v num = Map.insert v (newType num)

        deducer :: Map.Map String Type -> Int -> Expression -> Maybe (Int, TypeProof)
        deducer mapp num v@(Variable s) = Just $ if Map.member s mapp
            then (num,     VariableRule v # mapp)
            else (num + 1, VariableRule v # updateMap s num mapp)

        deducer mapp num l@(Lambda v e) = process <$> deducer (updateMap v num mapp) (num + 1) e
            where
                process (i, tp) = (
                        i + 1, LambdaRule l (newLambda i) (rule tp) #
                        Map.insert (newLambda i) (FunctionType (types tp Map.! v) (getType tp)) (Map.delete v (types tp))
                    )

        deducer mapp num a@(Application l r) = join $ processFirst <$> deducer mapp num l
            where
                processFirst (i, tp) = join $ (processSecond tp) <$> deducer (types tp) i r

                processSecond leftp (n, rightp) = case getType (rule leftp # types rightp) of
                    AtomicType s -> if cycled s $ getType rightp then Nothing else
                        Just (
                            n + 1, on (ApplicationRule a) rule leftp rightp #
                            Map.map (typeRename s (newFuncType rightp n)) (types rightp)
                        )

                    FunctionType lf _ -> process <$> superResolve lf (getType rightp)
                        where
                            process mapp' = (
                                    n, on (ApplicationRule a) rule leftp rightp #
                                    Map.map (\x -> iterate (flip (Map.foldrWithKey' typeRename) mapp') x !! (Map.size mapp' + 1)) (types rightp)
                                )


                newFuncType x i = FunctionType (getType x) (newType i)

                typeRename was new t@(AtomicType s)     = if s == was then new else t
                typeRename was new (FunctionType l' r') = on FunctionType (typeRename was new) l' r'

                cycled key (AtomicType at)      = at == key
                cycled key (FunctionType fl fr) = on (||) (cycled key) fl fr

                resolve :: Map.Map String Type -> Type -> Type -> Map.Map String Type
                resolve mapp' at@(AtomicType s) rightType = if at /= rightType then Map.insert s rightType mapp' else mapp'
                resolve mapp' leftType at@(AtomicType s)  = if at /= leftType  then Map.insert s leftType  mapp' else mapp'
                resolve mapp' (FunctionType l' r') (FunctionType l'' r'') = resolve (resolve mapp' l' l'') r' r''

                superResolve :: Type -> Type -> Maybe (Map.Map String Type)
                superResolve x y = check $ resolve Map.empty x y
                    where
                        check mapp' = if Map.foldlWithKey' (\was key val -> was && not (cycled key val)) True mapp'
                            then Just mapp' else Nothing
