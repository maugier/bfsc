{-
 - Symbolic expressions
 -}

module BFSC.Expr where

import Data.Map
import Data.Maybe (isJust)
import Data.List

import Control.Monad

newtype Var f = Var (Map [String] f)
    deriving (Eq, Ord)

mkVar :: (Num t, Eq t) => Map [String] t -> Var t
mkVar = Var . Data.Map.filter (/= 0)

constant :: t -> Var t
constant x = Var (singleton [] x)


toConstant :: (Num t) => Var t -> Maybe t
toConstant (Var x) = case keys x of
    []   -> Just 0
    [[]] -> Just (x ! [])
    _    -> Nothing

isConstant :: (Num t) => Var t -> Bool
isConstant = isJust . toConstant 

var :: Num t => String -> Var t
var s = Var (singleton [s] 1)

substitute :: (Num t, Eq t) => String -> t -> Var t -> Var t
substitute var val (Var x) = mkVar . fromListWith (+) . fmap subst . toList $ x where
    subst (ks, v) = (others, v * product (replicate (length good) val)) where
        (good, others) = Data.List.partition (var ==) ks

instance (Eq t, Num t) => Num (Var t) where
    fromInteger = constant . fromInteger
    negate (Var x) = Var (fmap negate x)
    Var a + Var b = mkVar $ unionWith (+) a b
    Var a * Var b = mkVar $ fromListWith (+) [ (sort (ka ++ kb), va*vb) | (ka,va) <- toList a, (kb,vb) <- toList b ]
    abs = undefined
    signum = undefined

instance (Show t, Num t, Eq t) => Show (Var t) where
    show (Var x) | Data.Map.null x = "0"
                 | otherwise = concat $ intersperse "+" [ (showMonom v k) | (k,v) <- toList x ]

showMonom v [] = show v
showMonom 1 ks = concat $ intersperse "*" ks
showMonom v ks = concat $ intersperse "*" (show v : ks)

{-  Represent partial information about expressions  -}

freeVariables :: Var t -> [String]
freeVariables (Var x) = nub . sort . concat . keys $ x


{- Retourne une liste de substitutions possibles si la contrainte est satisfaite. 
 - Nothing représente aucune substitution; en termes opérationnels, Nothing
 - doit être considéré comme équivalent à (Just id).
 - -}
solveSimple :: (Eq t, Num t, MonadPlus m) => Var t -> m (Maybe (String, t))
solveSimple (Var x) = case toAscList x of
    [([], v)] | v /= 0  -> mzero      -- x = 0 pour x != 0, impossible, bailout
    [([], k), ([var], 1)] -> return (Just (var, negate k))
    [([], k), ([var], 255)] -> return (Just (var, k))
    _ -> return Nothing
    
