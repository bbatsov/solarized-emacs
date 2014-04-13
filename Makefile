EMACS ?= emacs
EMACSFLAGS =
CASK = cask

dev:
	$(CASK) install
	$(CASK) update

.PHONY: test
test :
	$(CASK) exec ecukes

.PHONY: clean
clean :
	rm -rf .cask # Clean packages installed for development

