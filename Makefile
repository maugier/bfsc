all:: bfputs bfrun bfsymrun

clean::
	rm *.o BFSC/*.o *.hi BFSC/*.hi bfputs bfrun

bfputs::
	ghc -O3 --make $@

bfrun:: 
	ghc -O3 --make $@

bfsymrun::
	ghc -O3 --make $@
