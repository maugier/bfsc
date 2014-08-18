import BFSC.Base

main = getContents >>= print . runBF . read

