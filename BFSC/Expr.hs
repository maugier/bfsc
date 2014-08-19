{-
 - Symbolic expressions
 -}

module BFSC.Expr where

import Data.Map
import Data.List

newtype Var f = Var (Map [String] f)

constant :: t -> Var t
constant x = Var (singleton [] x)

isConstant :: Var t -> Bool
isConstant (Var x) = keys x `elem` [[], [[]]]

fromConstant :: (Num t) => Var t -> Maybe t
fromConstant (Var x) = case keys x of
    []   -> Just 0
    [[]] -> Just (x ! [])
    _    -> Nothing

var :: Num t => String -> Var t
var s = Var (singleton [s] 1)

substitute :: Num t => String -> t -> Var t -> Var t
substitute var val (Var x) = Var . fromListWith (+) . fmap subst . toList $ x where
    subst (ks, v) = (others, v * product (replicate (length good) val)) where
        (good, others) = Data.List.partition (var ==) ks

instance (Eq t, Num t) => Num (Var t) where
    fromInteger = constant . fromInteger
    negate (Var x) = Var (fmap negate x)
    Var a + Var b = Var (Data.Map.filter (/= 0) $ unionWith (+) a b)
    Var a * Var b = Var $ fromListWith (+) [ (sort (ka ++ kb), va*vb) | (ka,va) <- toList a, (kb,vb) <- toList b ]
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
