all: simdriver simtester

simdriver: *.hs
	ghc --make -O3 -fspec-constr-count=1000 simdriver

simtester: *.hs
	ghc --make -O3 -fspec-constr-count=1000 simtester

clean:
	rm -f *.hi *.o *~ simdriver simtester
