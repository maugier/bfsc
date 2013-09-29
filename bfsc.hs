
import Data.Char
import Data.List
import Data.Word
import Control.Monad.RWS
import Data.Ord (comparing)
import qualified Data.Map as M

data BF = MLeft | MRight | Up | Down | Out | Loop [BF]
	deriving Eq

instance Show BF where
	show MLeft = "<"
	show MRight = ">"
	show Up = "+"
	show Down = "-"
	show Out = "."
	show (Loop bf) = "[" ++ show bf ++ "]"
	showList [] rest     = rest
	showList (x:xs) rest = show x ++ (showList xs rest)

readBF ('+':s) = [(Up, s)]
readBF ('-':s) = [(Down, s)]
readBF ('<':s) = [(MLeft, s)]
readBF ('>':s) = [(MRight, s)]
readBF ('.':s) = [(Out, s)]
readBF ('[':s) = [(Loop p, t) | (p, (']':t) ) <- readBFList s ]
readBF _       = []

readBFList [] = [([],[])]
readBFList s@(']':_) = [([],s)]
readBFList s = [ ((x:xs), u) | (x,t) <- readBF s, (xs,u) <- readBFList t ]

instance Read BF where
	readsPrec _ = readBF
	readList = readBFList

data BFMachine = BFMachine [Word8] [Word8]

showBy s = concat . intersperse s . map show

instance Show BFMachine where
	show (BFMachine (x:l) r) = showBy "|" (reverse l) ++ "[" ++ show x ++ "]" ++ showBy "|"  r

initBF = BFMachine [0] []

type BFExec = RWS () Char BFMachine

state_ f = state $ \s -> ((), f s)
peek = rws (\() s@(BFMachine (x:_) _) -> (x,s,""))

iLeft (BFMachine [x]   r) = BFMachine [0] (x:r)
iLeft (BFMachine (x:l) r) = BFMachine l   (x:r)

iRight (BFMachine l [])    = BFMachine (0:l) []
iRight (BFMachine l (x:r)) = BFMachine (x:l) r

execMany = mapM_ exec

exec MLeft  = state_ $ iLeft
exec MRight = state_ $ iRight
exec Up    = state_ $ \(BFMachine (x:l) r) -> BFMachine ((x+1):l) r
exec Down  = state_ $ \(BFMachine (x:l) r) -> BFMachine ((x-1):l) r
exec Out   = peek >>= \x -> writer ((), [chr (fromIntegral x)])
exec l@(Loop prog) = do
	x <- peek
	case x of 
		0 -> return ()
		_ -> execMany prog >> exec l

runBF bf = execRWS (execMany (code bf)) () initBF

data BFProg = BFProg { cost :: Int, code :: [BF] }
	deriving Eq

instance Read BFProg where
	readsPrec i s = [ (bfprog p, r) | (p,r) <- readsPrec i s ]

instance Show BFProg where
	show = show . code

bfprog bf = BFProg (ccost bf) bf where
	ccost = sum . map icost
	icost (Loop p) = ccost p + 2
	icost _        = 1

instance Monoid BFProg where
	mempty = BFProg 0 []
	mappend (BFProg l1 p1) (BFProg l2 p2) = BFProg (l1+l2) (p1++p2)

instance Ord BFProg where
	compare = comparing $ \(BFProg l _) -> l

reset :: BFProg
reset = read "[-]"

stop :: BFProg
stop = bfprog []

left = bfprog [MLeft]
right = bfprog [MRight]
out = bfprog [Out]
inc = bfprog [Up]
dec = bfprog [Down]
loop = bfprog . (:[]) . Loop


linear :: Word8 -> BFProg
linear x | x > 127   = bfprog (replicate (fromIntegral $ 256 - x ) Down)
         | otherwise = bfprog (replicate (fromIntegral x) Up)


mult a = bfprog [Loop ((MRight : code (linear a)) ++ [MLeft,Down]), MRight] 
--mult a = right <> linear a <> loop ( left 

linears = [ (x, linear x) | x <- [0 .. 255] ]

affines = [ (a*b+c, linear a <> mult b <> linear c) | a <- [2..16], b <- [a..255], c <- [0..a] ]

best = M.fromListWith min (linears ++ affines)

identities :: [BF] -> [BF]

identities (Up:Down:rest) = identities rest
identities (Down:Up:rest) = identities rest
identities (MLeft:MRight:rest) = identities rest
identities (MRight:MLeft:rest) = identities rest
identities (x:rest) = x : identities rest
identities [] = []

cuthead (MLeft: rest)  = cuthead rest
cuthead (MRight: rest) = cuthead rest
cuthead rest           = rest

efix f = head . head . dropWhile (\(a:b:_) -> a /= b) . tails . iterate f

optimize = cuthead . efix identities 

