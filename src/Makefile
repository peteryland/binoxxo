SHELL := /bin/bash
HFLAGS := -O2

all: test

test: binoxxo
	@for file in ../test/*.box; do ./binoxxo < "$$file" | diff -u "$${file/%\.box/.out}" - 2>&1; done

binoxxo: Main.hs Types.hs ValidQRows.hs
	ghc -o $@ ${HFLAGS} $<

ValidQRows.hs: mktable
	./$< > $@

mktable: MkTable.hs Types.hs
	ghc -o $@ ${HFLAGS} $<

clean:
	@rm -f binoxxo mktable ValidQRows.hs *.hi *.o

.PHONY: all test clean
