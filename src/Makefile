SHELL := /bin/bash

test: binoxxo
	@for file in ../test/*.box; do ./binoxxo < "$$file" | diff -u "$${file/%\.box/.out}" - 2>&1; done

binoxxo: Main.hs Types.hs
	ghc -o $@ -O2 $<

mktable: MkTable.hs Types.hs
	ghc -o $@ -O2 $<

clean:
	@rm -f binoxxo *.hi *.o
