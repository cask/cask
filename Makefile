CASK ?= cask
EMACS ?= emacs

SERVANT_DIR = 'servant/tmp'

all: test

test: unit ecukes

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes

start-server: $(SERVANT_DIR)
	${CASK} exec ${EMACS} -Q --load servant/app.el --batch > servant/tmp/servant.log 2>&1 &

stop-server:
	kill $$(cat $(SERVANT_DIR)/servant.pid)

$(SERVANT_DIR):
	@mkdir -p $(SERVANT_DIR)

.PHONY: start-server stop-server unit ecukes test all
