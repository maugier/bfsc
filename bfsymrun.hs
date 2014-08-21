import BFSC.Expr
import BFSC.SymbolicExec
import BFSC.Machine

import Control.Monad

import Data.Char
import Data.List
import Data.Map (toList)

showConstraint (cell, True) = show cell ++ " != 0"
showConstraint (cell, False) = show cell ++ " == 0"

showVarbind (var, val) = show var ++ " -> " ++ show val

showVar v = case toConstant v of
    Just val -> show $ chr (fromIntegral val)
    Nothing -> "$((" ++ show v ++ "))"

showRun (state, output) = do
    putStrLn "-----"
    putStr "Out: "
    putStrLn (concat . intersperse ", " $ (map showVar output))
    putStr "Known variables: "
    putStrLn (concat . intersperse ", " $ (map showVarbind (toList (syVarbinds state))))
    putStr "Constraints: "
    putStrLn (concat . intersperse ", " $ (map showConstraint (toList (syConstraints state))))
    putStr "Final State: "
    putStrLn (show (syMachine state))

main = do
    prog <- getContents
    forM_ (symbolicExec (read prog) initBF) showRun
        
