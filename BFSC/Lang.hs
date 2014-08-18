{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BFSC.Lang where

import Data.Char
import Data.List
import Data.Word
import Control.Monad.RWS
import Data.Ord (comparing)
import qualified Data.Map as M


{- Un datatype de tous les programmes BF bien formés. La définition récursive de Loop 
 - permet d'exclure les crochets déséquilibrés -}
data BF = MLeft | MRight | Up | Down | Out | Loop [BF] | Nop Char
	deriving Eq

{- Affichage du type BF, sans commentaires -}
instance Show BF where
	show MLeft = "<"
	show MRight = ">"
	show Up = "+"
	show Down = "-"
	show Out = "."
	show (Loop bf) = "[" ++ show bf ++ "]"
	show (Nop c) = [c]
	showList [] rest     = rest
	showList (x:xs) rest = show x ++ (showList xs rest)

{- Parsing d'un programme BF, utilisant la construction monadique ReadS à la main -}
readBF ('+':s) = [(Up, s)]
readBF ('-':s) = [(Down, s)]
readBF ('<':s) = [(MLeft, s)]
readBF ('>':s) = [(MRight, s)]
readBF ('.':s) = [(Out, s)]
readBF ('[':s) = [(Loop p, t) | (p, (']':t) ) <- readBFList s ] -- lecture des paires de crochets
readBF (x:s)   = [(Nop x, s)]
readBF _       = []

readBFList [] = [([],[])]
readBFList s@(']':_) = [([],s)]
readBFList s = [ ((x:xs), u) | (x,t) <- readBF s, (xs,u) <- readBFList t ]

-- on fait de ce parser l'instance standard pour Read
instance Read BF where
	readsPrec _ = readBF
	readList = readBFList

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



{- Définition du type BFProg, qui contient une liste d'instructions BF
 - et un calcul de la longueur du programme (en cache pour l'efficacité) -}
data BFProg = BFProg { cost :: Int, code :: [BF] }
	deriving Eq

{- Pour wrapper une liste d'instructions BF dans un BFProg, on calcule simplement le cout
 - comme la longeur apparente du programme -}
bfprog bf = BFProg (ccost bf) bf where
	ccost = length . show

{- Pour parser un BFProg, on parse une liste de BF et on calcule le coût après -}
instance Read BFProg where
	readsPrec i s = [ (bfprog p, r) | (p,r) <- readsPrec i s ]

{- Pour afficher un BFProg, on affiche seulement son code -}
instance Show BFProg where
	show = show . code

{- Les BFProg sont concaténatifs: on définit le programme vide, et la concaténation
 - de programmes (avec addition du coût)
 -
 - Ceci permet d'utiliser l'opérateur <> pour combiner des BFProgs -}
instance Monoid BFProg where
	mempty = BFProg 0 []
	mappend (BFProg l1 p1) (BFProg l2 p2) = BFProg (l1+l2) (p1++p2)

{- Définition d'un ordre naturel pour les BFProg.
 - On les compare suivant le cout, ce qui permet d'obtenir facilement
 - le moins long des programmes via un appel aux fonction min ou minimum -}
instance Ord BFProg where
	compare = comparing cost
