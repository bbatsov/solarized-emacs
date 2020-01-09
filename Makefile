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
	$(CASK) exec emacs -q -l ./dev-emacs.d/init.el ${ARGS}

test: elpa
	$(MAKE) run

dev: clean elpa
	$(MAKE) ARGS="dev-emacs.d/test-files/* dev-emacs.d/init.el *-theme.el solarized*.el" run

dev-merge: clean elpa
	rm -rf dev-emacs.d/test-repo
	git clone dev-emacs.d/test.git dev-emacs.d/test-repo
	git -C dev-emacs.d/test-repo checkout v2
	git -C dev-emacs.d/test-repo merge origin/v1 || exit 0
	$(MAKE) ARGS="dev-emacs.d/init.el *-theme.el solarized.el dev-emacs.d/test-repo" run
