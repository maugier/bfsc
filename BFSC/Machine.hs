{-# LANGUAGE FlexibleContexts #-}

module BFSC.Machine where

import Data.List (intersperse)
import Control.Monad.State

{- Un datatype pour la machine virtuelle BF. Il s'agit d'un zipper, la première
 - liste représente les éléments à gauche dans l'ordre inverse, le premier élément
 - est l'élément courant; la deuxième liste représente les éléments à droite, dans l'ordre normal.
 -
 - On aurait pu utiliser des listes infinies remplies de 0, mais ne pas le faire permet de savoir
 - jusqu'ou la mémoire à été initialisée. -}
data BFMachine w = BFMachine [w] [w]

-- fonction pour afficher une liste avec un délimiteur choisi, utile ci-dessous
showBy s = concat . intersperse s . map show

{- Affichage de l'état de la machine BF: les éléments de gauche (dans l'ordre inverse), 
 - puis l'élément courant, puis les éléments de droite, avec des () autour de l'élément
 - courant et des | comme séparateurs.
 -
 - Exemple: 0|1|2(3)4|5 -}

instance Show w => Show (BFMachine w) where
	show (BFMachine (x:l) r) = showBy "|" (reverse l) ++ "(" ++ show x ++ ")" ++ showBy "|"  r

{- Appliquer une transformation à toutes les cellules initialisées.
 - Utiliser uniquement avec des fonctions préservant le 0. -}
instance Functor BFMachine where
    fmap f (BFMachine xs ys) = BFMachine (fmap f xs) (fmap f ys)


{- Utile pour les fonctions linéaires -}
lzip _ xs [] = xs
lzip _ [] ys = ys
lzip f (x:xs) (y:ys) = f x y : lzip f xs ys

BFMachine al ar |+| BFMachine bl br = BFMachine (lzip (+) al bl) (lzip (+) ar br)



-- Etat initial de la machine BF: une seule case à 0
initBF :: Num w => BFMachine w
initBF = BFMachine [0] []
-- helper, state_ est comme state mais en ignorant la valeur de retour
state_ :: (MonadState s m) => (s -> s) -> m ()
state_ f = get >>= put . f

-- Opération monadique pour lire la valeur de la cellule en cours
peek :: (MonadState (BFMachine w) m) => m w
peek = get >>= \(BFMachine (x:_) _) -> return x

{- Déplacement du pointeur à gauche/à droite: on copie le premier
 - élément de la liste gauche/droite en tête de l'autre liste. Au
 - cas ou une cellule n'est pas initialisée, on l'initialise à 0 maintenant -}
iLeft (BFMachine [x]   r) = BFMachine [0] (x:r)
iLeft (BFMachine (x:l) r) = BFMachine l   (x:r)

iRight (BFMachine l [])    = BFMachine (0:l) []
iRight (BFMachine l (x:r)) = BFMachine (x:l) r

{- Tests pour savoir si le pointeur est au bout de la bande -}
leftmost (BFMachine [_] _) = True
leftmost _                 = False

rightmost (BFMachine _ []) = True
rightmost _                = False

onPtr :: (w -> w) -> BFMachine w -> BFMachine w
onPtr f (BFMachine (x:xs) ys) = BFMachine (f x : xs) ys

getPtr (BFMachine (x:_) _) = x
