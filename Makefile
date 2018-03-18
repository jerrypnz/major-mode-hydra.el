CASK ?= cask
EMACS ?= emacs

.PHONY: build test clean dist

build:
	${CASK} clean-elc
	${CASK} build

test:
	${CASK} clean-elc
	${CASK} exec ert-runner

clean:
	${CASK} clean-elc

dist:
	${CASK} package
