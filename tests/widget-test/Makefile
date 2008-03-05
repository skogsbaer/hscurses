# PROF_OPTS = -prof -auto-all
GHC = ghc --make $(PROF_OPTS) -package-conf $(PKG_CONF) -package hscurses
BINS = edit-test text-test table-test
PKG_CONF = ../../hscurses.cabal

default: text-test

all: $(BINS)

text-test: TextTest.hs
	$(GHC) -o $@ $<

table-test: TableTest.hs
	$(GHC) -o $@ $<

edit-test: EditTest.hs
	$(GHC) -o $@ $<

clean: 
	rm -rf *.o *.hi $(BINS)

