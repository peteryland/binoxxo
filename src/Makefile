SHELL := /bin/bash
HFLAGS := -O2

all: binoxxo

test: binoxxo
	@for file in ../test/*.box; do diff -u "$${file/%\.box/.out}" <(./binoxxo < "$$file") 2>&1 && echo $$(basename $$file): OK.; done

binoxxo: Main.hs Types.hs ValidQRows.hs
	ghc -o $@ ${HFLAGS} $<

ValidQRows.hs: mktable
	./$< > $@

mktable: MkTable.hs Types.hs
	ghc -o $@ ${HFLAGS} $<

clean:
	@rm -f binoxxo mktable ValidQRows.hs *.hi *.o

.PHONY: all test clean
