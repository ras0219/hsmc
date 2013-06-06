all: calculator gtcalculator

%: %.hs
	ghc --make $<

