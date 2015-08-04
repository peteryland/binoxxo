SHELL := /bin/bash
HFLAGS := -O2
MAXLEN ?= 14
maxlen := $(shell tr -cd '[:digit:]' <<< "${MAXLEN}")

all: binoxxo

test: binoxxo
	@for file in ../test/*.box; do diff -u "$${file/%\.box/.out}" <(./binoxxo < "$$file") 2>&1 && echo $$(basename $$file): OK.; done

binoxxo: Main.hs Types.hs ValidQRows.hs
	ghc -o $@ ${HFLAGS} $<

ValidQRows.hs: mktable
	./$< ${maxlen} > $@

mktable: MkTable.hs Types.hs
	ghc -o $@ ${HFLAGS} $<

clean:
	@rm -f binoxxo mktable ValidQRows.hs *.hi *.o

install: binoxxo
	@cp -f binoxxo /usr/bin/

.PHONY: all test clean install
