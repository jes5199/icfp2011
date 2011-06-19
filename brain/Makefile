all: braintester braindriver genTestCases

braintester: *.hs ../*.hs
	ghc -i../simulator --make -O3 braintester

braindriver: *.hs ../*.hs
	ghc -i../simulator --make -O3 braindriver

genTestCases: *.hs ../*.hs
	ghc -i../simulator --make -O3 genTestCases

playerOfGames: *.hs ../*.hs
	ghc -i../simulator --make -O3 playerOfGames
