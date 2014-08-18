import BFSC.Exec

main = getContents >>= print . runBF . read

