
import Data.Char
import Data.List
import Data.Word
import Control.Monad.RWS

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

instance Show BFMachine where
	show (BFMachine (x:l) r) = concatMap show (reverse l) ++ "[" ++ show x ++ "]" ++ concatMap show r

initBF = BFMachine [0] []

type BFExec = RWS () Char BFMachine

state_ f = state $ \s -> ((), f s)
peek = rws (\() s@(BFMachine (x:_) _) -> (x,s,""))

left (BFMachine [x]   r) = BFMachine [0] (x:r)
left (BFMachine (x:l) r) = BFMachine l   (x:r)

right (BFMachine l [])    = BFMachine (0:l) []
right (BFMachine l (x:r)) = BFMachine (x:l) r

execMany = mapM_ exec

exec MLeft  = state_ $ left
exec MRight = state_ $ right
exec Up    = state_ $ \(BFMachine (x:l) r) -> BFMachine ((x+1):l) r
exec Down  = state_ $ \(BFMachine (x:l) r) -> BFMachine ((x-1):l) r
exec Out   = peek >>= \x -> writer ((), [chr (fromIntegral x)])
exec l@(Loop prog) = do
	x <- peek
	case x of 
		0 -> return ()
		_ -> execMany prog >> exec l


runBF bf = execRWS (execMany bf) () initBF

reset :: [BF]
reset = read "[-]"

linear :: Word8 -> [BF]
linear x = replicate (fromIntegral x) Up



identities :: [BF] -> [BF]

identities (Up:Down:rest) = identities rest
identities (Down:Up:rest) = identities rest
identities (MLeft:MRight:rest) = identities rest
identities (MRight:MLeft:rest) = identities rest
identities (x:rest) = x : identities rest
identities [] = []

efix f = head . head . dropWhile (\(a:b:_) -> a /= b) . tails . iterate f

optimize = efix identities 
