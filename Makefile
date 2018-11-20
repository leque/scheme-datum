EMACS ?= emacs
CASK ?= cask
LOADPATH = -L .
ELPA_DIR = \
    $(shell emacs=$(EMACS) $(CASK) package-directory)

scheme-datum.el: README.org
	$(CASK) $(EMACS) --batch \
		-l org-commentary-cli \
		-eval '(setq org-ascii-links-to-notes nil)' \
		-f org-commentary -- $< $@

.PHONY: test
test: $(ELPA_DIR)
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test.el \
		-f ert-run-tests-batch-and-exit

$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
