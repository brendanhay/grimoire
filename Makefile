CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: build conf clean

all:
	$(CABAL) install

build:
	$(CABAL) build

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean
