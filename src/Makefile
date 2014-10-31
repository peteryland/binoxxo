SHELL := /bin/bash

test: binoxxo
	@for file in ../test/*.box; do ./binoxxo < "$$file" | diff -u "$${file/%\.box/.out}" - 2>&1; done

binoxxo: Makefile binoxxo.hs
	ghc binoxxo.hs
	@rm -f binoxxo.hi binoxxo.o

clean:
	@rm -f binoxxo
