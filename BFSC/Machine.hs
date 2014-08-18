module BFSC.Machine where

import Data.List (intersperse)

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

-- Etat initial de la machine BF: une seule case à 0
initBF :: Num w => BFMachine w
initBF = BFMachine [0] []
