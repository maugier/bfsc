{-# LANGUAGE NoMonomorphismRestriction #-}

module BFSC.SymbolicExec where

import BFSC.Lang
import BFSC.Expr
import BFSC.Machine

import Control.Monad.RWS
import Control.Arrow
import Data.Char (chr)
import Data.Word
import Data.Map

type Cell = Var Word8

{- Sets of allowable values -}
type Constraints = Map Cell Bool
type Varbinds    = Map String Word8
data SymbolicState = SymbolicState { syConstraints :: Constraints,
                                     syVarbinds    :: Varbinds,
                                     syMachine     :: BFMachine Cell }
    deriving Show


type SymbolicRun = RWST () [Cell] SymbolicState []

onMachine f     = state_ (\(SymbolicState c v m) -> SymbolicState c v (f m))
onBinds f       = state_ (\(SymbolicState c v m) -> SymbolicState c (f v) m)
onConstraints f = state_ (\(SymbolicState c v m) -> SymbolicState (f c) v m)


bind :: String -> Word8 -> SymbolicRun ()
bind var val = do
    onBinds (insert var val)
    onMachine (fmap (substitute var val))

assert :: Cell -> Bool -> SymbolicRun ()
assert cell True = onConstraints (insert cell True) 
assert cell False = do  
    sols <- solveSimple cell
    case sols of
        Nothing -> return ()
        Just (var,val) -> bind var val



{-
assert cell val = do 
                | otherwise = onBinds (insert cell val) -}


execMany = mapM_ exec

exec :: BF -> SymbolicRun ()
exec Out = do { st <- get; tell [getPtr (syMachine st)] }
exec MLeft = onMachine iLeft
exec MRight = onMachine iRight
exec Up = onMachine $ onPtr (+1)
exec Down = onMachine $ onPtr (subtract 1)
exec (Loop [Down]) = onMachine $ onPtr (const 0) -- resets
exec (Loop [Up])   = onMachine $ onPtr (const 0)
exec (Loop loop) | isLinear loop = execLinearLoop loop
                 | otherwise = execLoop loop

execLoop :: [BF] -> SymbolicRun ()
execLoop loop = do
    st <- get
    let ptr = getPtr (syMachine st)
    case toConstant ptr of
        Just 0 -> return () -- loop is known not to execute
        Just _ -> execMany loop >> execLoop loop -- loop is known to execute
        Nothing -> (assert ptr False) `mplus` (assert ptr True >> execMany loop >> execLoop loop) -- branch

execLinearLoop :: [BF] -> SymbolicRun ()
execLinearLoop loop = do
    let [(st, _)] = symbolicExec loop initBF
    let result = syMachine st
    machine <- fmap syMachine get
    let cur = getPtr machine :: Cell
    onMachine $ (|+| fmap (* cur) result)

symbolicExec :: [BF] -> BFMachine Cell -> [(SymbolicState, [Cell])]
symbolicExec program init = execRWST (execMany program) () (SymbolicState empty empty init)

linearOp MLeft = True
linearOp MRight = True
linearOp Up = True
linearOp Down = True
linearOp (Nop _) = True
linearOp _ = False

balance MLeft = -1
balance MRight = 1
balance _ = 0

isLinear prog = all linearOp prog && sum (fmap balance prog) == 0

