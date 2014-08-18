module BFSC.SymbolicExec where

import BFSC.Lang
import BFSC.Expr
import BFSC.Machine

import Control.Monad.RWS
import Data.Word
import Data.Map

type Cell = Var Word8

type VarBinds = Map String (Either Word8 Word8)

type SymbolicState = (VarBinds, BFMachine Cell)

type SymbolicRun = RWST () String SymbolicState []

execMany = mapM_ exec

exec :: BF -> SymbolicRun ()
exec = undefined
