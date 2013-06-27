all: calculator gtcalculator thaumcraft

%: %.hs
	ghc --make $<

