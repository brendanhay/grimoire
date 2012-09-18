CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: build conf clean

all: build

build:
	$(CABAL) install

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean
