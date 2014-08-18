{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import BFSC.Compile
import System.Environment

main = getArgs >>= print . compile . vectorPrint . unwords
