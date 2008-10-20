all: gat
gat: *.hs
	ghc --make -o gat Gat.hs
clean:
	rm -f *.hi *.o RevParse_test gat
opt:
	ghc --make -o gat -O2 Gat.hs
profile:
	ghc -prof -auto-all --make -o gat Gat.hs

# Cut'n'paste job from Cabal.
doc:
	haddock --html --odir=html/ --title=gat --read-interface=/usr/share/doc/ghc6-doc/libraries/base,/usr/share/doc/ghc6-doc/libraries/base/base.haddock --read-interface=/usr/share/doc/ghc6-doc/libraries/bytestring,/usr/share/doc/ghc6-doc/libraries/bytestring/bytestring.haddock --read-interface=/usr/share/doc/ghc6-doc/libraries/directory,/usr/share/doc/ghc6-doc/libraries/directory/directory.haddock --read-interface=/usr/share/doc/ghc6-doc/libraries/filepath,/usr/share/doc/ghc6-doc/libraries/filepath/filepath.haddock --read-interface=/usr/share/doc/ghc6-doc/libraries/mtl,/usr/share/doc/ghc6-doc/libraries/mtl/html/mtl.haddock --read-interface=/usr/share/doc/ghc6-doc/libraries/process,/usr/share/doc/ghc6-doc/libraries/process/process.haddock --read-interface=/usr/share/doc/ghc6-doc/libraries/unix,/usr/share/doc/ghc6-doc/libraries/unix/unix.haddock `ls *.hs | grep -v Gat`
