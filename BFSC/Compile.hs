{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BFSC.Compile where

import BFSC.Lang
import BFSC.Exec
import BFSC.Machine

import Data.Char
import Data.List
import Data.Word
import Control.Monad.RWS
import Data.Ord (comparing)
import qualified Data.Map as M
import System.Environment

{- Un type monadique pour représenter la compilation de code. L'état est une machine BFMachine,
 - mais le Writer ne correspond plus aux outputs de la machine, mais au code produit par le compilateur.
 - On peut donc émettre du code dans un but précis, en tenant en compte les états initiaux et finaux de la machine -}
type BFCompile = RWST () BFProg (BFMachine Word8) []

{- Une fonction dans la monade BFCompile pour émettre un morceau de code fixe.
 - on lit l'état courant de la machine, on exécute le morceau de code, et on
 - enregistre le nouvel état de la machine, sans oublier d'ajouter le code
 - au programme courant. -}
emit :: BFProg -> BFCompile ()
emit prog = do
	st <- get
	let (st',_) = execRWS (execMany . code $ prog) () st 
	put st'
	writer ((), prog) 

trd (_,_,x) = x

{- Elimine localement les solutions les moins bonnes dans le
 - contexte d'un BFCompile -}
prune :: Int -> BFCompile () -> BFCompile ()
prune n = RWST . (((take n . sortBy (comparing trd)).).)  .  runRWST

{- Un BFProg qui réalise l'addition naive par une constante, en répétant
 - n fois un + ou un - -}
linear :: Word8 -> BFProg
linear x | x > 127   = bfprog (replicate (fromIntegral $ 256 - x ) Down)
         | otherwise = bfprog (replicate (fromIntegral x) Up)

{- Un BFCompile produisant une valeur fixe dans la cellule courante,
 - a partir de la valeur précédente, en utilisant une fonction d'addition
 - par une constante -}
useRelative :: (Word8 -> BFProg) -> Word8 -> BFCompile ()
useRelative f t = do            -- f est une fonction d'addition, et t la valeur cible
	x <- peek               -- on lit l'état de la cellule courante x
	emit $ f (t - x) <> out -- code émis: on applique une addition par (t-x), puis on appelle l'instruction de sortie

-- Un BFCompile qui produit une valeur par addition naive
useLinear = useRelative linear

{- Un BFProg qui effectue une multiplication par une constante a, en
 - supposant que la cellule à la droite de la cellule courante vaut 0 -}
mult a = loop (right <> linear a <> left <> dec)

idiv a = loop (right <> inc <> left <> linear (negate a))

{- Un BFProg qui effectue une multiplication sur un vecteur -}

vmult v = loop (expand v <> dec) where
	expand [] = mempty
	expand (x:xs) = right <> linear x <> expand xs <> left

{- Toutes les manières d'obtenir une addition par x en utilisant l'addition linéaire naive -}
linears = [ (x, linear x) | x <- [0 .. 255] ]

{- Toutes les transformations affines sur une valeur -}
affines = [ (a*b+c, left <> linear a <> mult b <> right <> linear c) | a <- [2..16], b <- [a..255], c <- [0..a] ]

inverse_affines = [ (a+c , left <> linear (a*b) <> idiv b <> right <> linear c) | a <- [1..255], b <- [2..15], (a*b) < 15,  c <- [0..255]] 

{- Une table des meilleures techniques pour effectuer une addition par n, en considérant soit une addition linéaire,
 - soit une multiplication suivie d'une addition. On utilise simplement la fonction min pour éliminer les doublons
 - en gardant le code le plus court -}
bestTable = M.fromListWith min (linears ++ affines ++ inverse_affines)

best = (bestTable M.!)

affines' = [(a*b+c, left <> best a <> mult b <> right <> best c) | a <- [2..16], b <- [a..255], c <- [0..a]]

bestTable' = M.fromListWith min (linears ++ affines ++ affines')

{- Un BFCompile qui produit une valeur par addition relative d'une des techniques précitées -}
useBest :: Word8 -> BFCompile ()
useBest = useRelative best

useMult y = do
	x <- peek
	if x > y then mzero
	         else emit (mult (x `div` y) <> best (x `mod` y))

{- Un BFCompile qui produit un string, en séquencant simplement useBest sur la 
 - valeur ascii de chaque caracère -}
naivePrint :: String -> BFCompile ()
naivePrint = mapM_ (useBest . fromIntegral . ord)

{- Une routine qui initialise un tableau bien adapté au texte ASCII -}
vectorStart = emit (best 18 <> vmult [2,4,5,6]) `mplus` 
              emit (best 32 <> vmult [1,2,3])

{- Un BFCompile qui utilise la stratégie linéaire sur l'array existant, minimisant
 - la taille du code à chaque étape intermédiaire -}

vectorPrintChar c = pickSomeCell >> useLinear (fromIntegral.ord $ c)

vectorPrint' []     = return ()
vectorPrint' (x:xs) = prune 1 (vectorPrintChar x) >> vectorPrint' xs 

vectorPrint :: String -> BFCompile ()
vectorPrint = (vectorStart >>) . vectorPrint'

{- Un déplacement non-déterministe vers une cellule initialisée -}

pickSomeCell :: BFCompile ()
pickSomeCell = return () `mplus` pickLeft `mplus` pickRight

pickLeft = do
	s <- get
	if leftmost s
		then mzero
		else emit left >> (return () `mplus` pickLeft)

pickRight = do
	s <- get
	if rightmost s
		then mzero
		else emit right >> (return () `mplus` pickRight)


{- Une passe d'optimisation naive qui élimine les opérations inverses -}
identities :: [BF] -> [BF]

identities (Up:Down:rest) = identities rest
identities (Down:Up:rest) = identities rest
identities (MLeft:MRight:rest) = identities rest
identities (MRight:MLeft:rest) = identities rest
identities (x:rest) = x : identities rest
identities [] = []

{- Une passe d'optimisation naive qui élimine les mouvements du pointeur en début de programme -}
cuthead (MLeft: rest)  = cuthead rest
cuthead (MRight: rest) = cuthead rest
cuthead rest           = rest

{- Une fonction qui applique une passe jusqu'a ce que le programme ne change plus -}
efix f = head . head . dropWhile (\(a:b:_) -> a /= b) . tails . iterate f

{- On applique nos deux passes sur le code pour optimiser un BFProg -}
optimize = bfprog . cuthead . efix identities . code

{- Exécution d'un BFCompile pour en extraire le code, et optimisation du code -}
compile :: BFCompile () -> BFProg
compile prog  = minimum . map (optimize . snd) $ execRWST prog () initBF


{- Exemple:
 -
 - *Main> compile $ naivePrint "hello world !"
   ++++++++[>+++++++++++++<-]>.---.+++++++..+++.<++++++++++[>--------<-]>+.<+++++++[>++++++++++++<-]>+++.--------.+++.------.--------.<++++++++++[>-------<-]>++.+.
 -
 - *Main> compile $ vectorPrint "a B c"
   ++++[>++++++++<-]>[>+>++>+++<<<-]>>>+.<<.>++.<.>>++.

 -
 -}
