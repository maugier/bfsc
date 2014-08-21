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

{- Sets of allowable values -}
type Constraints = Map Cell Bool
type SymbolicState = (Constraints, BFMachine Cell)


type SymbolicRun = RWST () [Cell] SymbolicState []

onMachine = state_ . second
onBinds = state_ . first


assert :: Cell -> Bool -> SymbolicRun ()
assert cell val = onBinds (insert cell val)


execMany = mapM_ exec

exec :: BF -> SymbolicRun ()
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
    (_, machine) <- get
    let ptr = getPtr machine
    case toConstant ptr of
        Just 0 -> return () -- loop is known not to execute
        Just _ -> execMany loop >> execLoop loop -- loop is known to execute
        Nothing -> (assert ptr False) `mplus` (assert ptr True >> execMany loop >> execLoop loop) -- branch

execLinearLoop :: [BF] -> SymbolicRun ()
execLinearLoop loop = do
    let [((_,result), _)] = symbolicExec loop initBF
    machine <- fmap snd get
    let cur = getPtr machine :: Cell
    onMachine $ (|+| fmap (* cur) result)

symbolicExec :: [BF] -> BFMachine Cell -> [(SymbolicState, [Cell])]
symbolicExec program init = execRWST (execMany program) () (empty, init)

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

