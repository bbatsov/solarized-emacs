CASK = cask
export EMACS ?= emacs
EMACSFLAGS =
TESTFLAGS =

SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: compile test clean elpa

all: compile

elpa-$(EMACS):
	$(CASK) install
	$(CASK) update
	touch $@

elpa: elpa-$(EMACS)

elpaclean:
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development

compile: elpa
	$(CASK) build

clean:
	rm -f $(OBJS)

# test: elpa
# 	$(CASK) exec buttercup -L .

run:
	$(CASK) exec emacs -q -l ./minimal-init/init.el ${ARGS}

test: elpa
	$(MAKE) run

dev: clean elpa
	$(MAKE) ARGS="minimal-init/test-files/* minimal-init/init.el *-theme.el solarized.el" run
