GHC = ghc
PKG_CONF = ../hscurses.conf

cm: ContactManager.hs $(PKG_CONF)
	$(GHC) --make -package-conf $(PKG_CONF) -package hscurses -o $@ $<

.PHONY: clean
clean:
	rm -f *.o *.hi cm	
