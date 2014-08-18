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
exec _             = error "Unsupported program for symbolic execute"

symbolicExec :: [BF] -> BFMachine Cell -> [(SymbolicState, [Cell])]
symbolicExec program init = execRWST (execMany program) () (empty, init)
