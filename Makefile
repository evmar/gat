all: gat tests
gat: *.hs
	ghc --make -o gat Gat.hs
tests: RevParse_test
RevParse_test: RevParse*.hs
	ghc --make -o RevParse_test RevParse_test.hs
clean:
	rm -f *.hi *.o RevParse_test gat
opt:
	ghc --make -o gat -O2 Gat.hs
