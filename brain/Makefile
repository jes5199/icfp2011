all: braintester braindriver genTestCases

braintester: *.hs
	ghc -i../simulator --make -O3 braintester

braindriver: *.hs
	ghc -i../simulator --make -O3 braindriver

genTestCases: *.hs
	ghc -i../simulator --make -O3 genTestCases

playerOfGames: *.hs
	ghc -i../simulator --make -O3 playerOfGames
