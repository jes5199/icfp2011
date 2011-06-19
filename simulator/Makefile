all: simdriver simtester braintester braindriver genTestCases

simdriver: *.hs
	ghc --make -O3 -fspec-constr-count=10000 simdriver

simtester: *.hs
	ghc --make -O3 -fspec-constr-count=10000 simtester

braintester: *.hs ../simulator/*.hs
	ghc --make -O3 braintester

braindriver: *.hs ../simulator/*.hs
	ghc --make -O3 braindriver

genTestCases: *.hs ../simulator/*.hs
	ghc --make -O3 genTestCases

clean:
	rm -f *.hi *.o *~ simdriver simtester braintester braindriver genTestCases
