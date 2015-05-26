DIRS = src

all:
	        @for d in $(DIRS); do $(MAKE) -C "$$d"; done

test:
	        @for d in $(DIRS); do $(MAKE) -C "$$d" test; done

clean:
	        @for d in $(DIRS); do $(MAKE) -C "$$d" clean; done

.PHONY: all test clean
