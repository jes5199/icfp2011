all: simdriver simtester

simdriver: *.hs
	ghc --make -O3 simdriver

simtester: *.hs
	ghc --make -O3 simtester

clean:
	rm -f *.hi *.o *~
