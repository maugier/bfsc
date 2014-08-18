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
data BF = MLeft | MRight | Up | Down | In | Out | Loop [BF] | Nop Char
	deriving Eq

{- Affichage du type BF, sans commentaires -}
instance Show BF where
	show MLeft = "<"
	show MRight = ">"
	show Up = "+"
	show Down = "-"
	show In = ","
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
readBF (',':s) = [(In, s)]
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
