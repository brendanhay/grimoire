CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: build conf clean

all:
	$(CABAL) build

build:
	$(CABAL) install

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean
	rm -rf .cache tmp cookbooks
