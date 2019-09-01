EMACS ?= $(shell which emacs)
SRC=$(shell git ls-files *.el)
ELCFILES = $(SRC:.el=.elc)
.DEFAULT_GOAL := test-compile

.PHONY: test-compile
test-compile:
	! ( $(EMACS) -Q --batch -l package --eval "(setq byte-compile-error-on-warn t)" \
	      --eval "(package-initialize)" -f batch-byte-compile $(SRC) 2>&1 \
	      | egrep -a "(Warning|Error):" )
	rm -f $(ELCFILES)
