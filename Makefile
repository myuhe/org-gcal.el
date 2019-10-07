EMACS ?= $(shell which emacs)
SRC=org-gcal.el
TEST=org-gcal-test.el
LOAD_PATH=.load-path.el
ELCFILES = $(SRC:.el=.elc)
.DEFAULT_GOAL := all

.PHONY: all clean load-path test-compile test

all: test-compile test

clean:
	rm -f $(ELCFILES) $(LOAD_PATH)

test-compile:
	! ( $(EMACS) -Q --batch -l package --eval "(setq byte-compile-error-on-warn t)" \
	      --eval "(package-initialize)" -f batch-byte-compile $(SRC) 2>&1 \
	      | egrep -a "(Warning|Error):" )
	rm -f $(ELCFILES)

test: $(SRC) $(TEST) $(LOAD_PATH)
	$(EMACS) -Q --batch -l $(LOAD_PATH) -l ert -l $(TEST) -l package \
	      --eval "(package-initialize)" -f ert-run-tests-batch-and-exit

load-path: $(LOAD_PATH)

$(LOAD_PATH):
	$(EMACS) --batch --eval "(package-initialize)" \
	         --eval "(pp \`(setq load-path (list ,@load-path)))" > $(LOAD_PATH)
