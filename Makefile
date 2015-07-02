DIRS = src

all:
	        @for d in $(DIRS); do $(MAKE) -s -C "$$d"; done

test:
	        @for d in $(DIRS); do $(MAKE) -s -C "$$d" test; done

clean:
	        @for d in $(DIRS); do $(MAKE) -s -C "$$d" clean; done

install:
	        @for d in $(DIRS); do $(MAKE) -s -C "$$d" install; done

.PHONY: all test clean install
