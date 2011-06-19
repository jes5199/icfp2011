all: braintester braindriver genTestCases

braintester: *.hs ../simulator/*.hs
	ghc -i../simulator --make -O3 braintester

braindriver: *.hs ../simulator/*.hs
	ghc -i../simulator --make -O3 braindriver

genTestCases: *.hs ../simulator/*.hs
	ghc -i../simulator --make -O3 genTestCases
