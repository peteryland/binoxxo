SHELL := /bin/bash

test: binoxxo
	@for file in ../test/*.box; do echo $${file##*/}:; time ./binoxxo < "$$file" | diff -u "$${file/%\.box/.out}" - 2>&1; echo Done.; done

binoxxo: Makefile binoxxo.hs
	ghc binoxxo.hs
	@rm -f binoxxo.hi binoxxo.o

clean:
	@rm -f binoxxo
