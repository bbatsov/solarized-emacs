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

dev-merge: clean elpa
	rm -rf minimal-init/test-repo
	git clone minimal-init/test.git minimal-init/test-repo
	git -C minimal-init/test-repo checkout v2
	git -C minimal-init/test-repo merge origin/v1 || exit 0
	$(MAKE) ARGS="minimal-init/init.el *-theme.el solarized.el minimal-init/test-repo" run
