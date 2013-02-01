all: calculator

calculator: calculator.hs
	ghc --make calculator.hs
