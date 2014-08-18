{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BFSC.Exec where

import BFSC.Lang
import BFSC.Machine

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

