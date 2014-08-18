{-# LANGUAGE NoMonomorphismRestriction #-}

module BFSC.SymbolicExec where

import BFSC.Lang
import BFSC.Expr
import BFSC.Machine

import Control.Monad.RWS
import Control.Arrow
import Data.Word
import Data.Map

type Cell = Var Word8

type VarBinds = Map String (Either Word8 Word8)

type SymbolicState = (VarBinds, BFMachine Cell)

type SymbolicRun = RWST () [Cell] SymbolicState []

onMachine = state_ . second

execMany = mapM_ exec

exec :: BF -> SymbolicRun ()
exec MLeft = onMachine iLeft
exec MRight = onMachine iRight
exec Up = onMachine $ onPtr (+1)
exec Down = onMachine $ onPtr (subtract 1)
exec (Loop [Down]) = onMachine $ onPtr (const 0)
exec (Loop [Up])   = onMachine $ onPtr (const 0)
exec (Loop loop) | isLinear loop = execLinearLoop loop
                 | otherwise = error "Unsupported program for symbolic execute"

execLinearLoop :: [BF] -> SymbolicRun ()
execLinearLoop loop = do
    let [((_,result), _)] = symbolicExec loop initBF
    machine <- fmap snd get
    let cur = getPtr machine :: Cell
    onMachine $ (|+| fmap (* negate cur) result)

symbolicExec :: [BF] -> BFMachine Cell -> [(SymbolicState, [Cell])]
symbolicExec program init = execRWST (execMany program) () (empty, init)

linearOp MLeft = True
linearOp MRight = True
linearOp Up = True
linearOp Down = True
linearOp (Nop _) = True

balance MLeft = -1
balance MRight = 1
balance _ = 0

isLinear prog = all linearOp prog && sum (fmap balance prog) == 0

