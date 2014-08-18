{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BFSC.Exec where

import BFSC.Lang

import Data.Char
import Data.List
import Data.Word
import Control.Monad.RWS
import Data.Ord (comparing)
import qualified Data.Map as M

{- Type monadique RWS représentant l'exécution d'instructions BF. 
 - pas de Reader (type ()), un Writer Char pour la sortie des caractères,
 - et un State BFMachine. -}
type BFExec = RWS () String (BFMachine Word8)

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

{- Exécution des instructions (type BF) au sein d'une State Monad sur BFMachine: -}

-- Pour exécuter plusieurs instructions, on séquence simplement leurs actions
execMany = mapM_ exec

-- Exécution individuelle de chaque opération
exec MLeft  = state_ $ iLeft
exec MRight = state_ $ iRight
exec Up    = state_ $ \(BFMachine (x:l) r) -> BFMachine ((x+1):l) r
exec Down  = state_ $ \(BFMachine (x:l) r) -> BFMachine ((x-1):l) r
exec Out   = do { x <- peek; tell [chr (fromIntegral x)] }
exec l@(Loop prog) = do   -- exécution d'une boucle
	x <- peek         -- on récupère la valeur du ptr courant
	case x of 
		0 -> return ()                -- si 0, stop
		_ -> execMany prog >> exec l  -- sinon, on exécute le contenu de la boucle, et on recommence
exec (Nop _) = return ()

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

{- Exécution rapide d'un BFProg, en produisant l'état final de la machine et la sortie.
 - on utilise simplement la monade RWS, en partant d'une machine mise à 0 -}
runBF :: BFProg -> (BFMachine Word8, String)
runBF bf = execRWS (execMany (code bf)) () initBF


{- Quelques définitions utilitaires de fonctions utiles -}

reset :: BFProg
reset = read "[-]"

stop :: BFProg
stop = mempty

left = bfprog [MLeft]
right = bfprog [MRight]
out = bfprog [Out]
inc = bfprog [Up]
dec = bfprog [Down]
loop = bfprog . (:[]) . Loop . code

