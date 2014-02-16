CASK ?= cask
EMACS ?= emacs
SERVANT ?= servant

SERVANT_DIR = 'servant'
SERVANT_TMP_DIR = $(SERVANT_DIR)/'tmp'

all: test

test: unit ecukes

unit:
	$(CASK) exec ert-runner

ecukes:
	$(CASK) exec ecukes

index:
	$(CASK) exec $(SERVANT) index --packages-path $(SERVANT_DIR)/packages
	$(CASK) exec $(SERVANT) index --packages-path $(SERVANT_DIR)/new-packages

start-server: $(SERVANT_DIR) index
	$(CASK) exec $(SERVANT) start > $(SERVANT_TMP_DIR)/servant.log 2>&1 &

stop-server:
	kill $$(cat $(SERVANT_TMP_DIR)/servant.pid)

$(SERVANT_DIR):
	@mkdir -p $(SERVANT_DIR)
	@mkdir -p $(SERVANT_TMP_DIR)

.PHONY: index start-server stop-server unit ecukes test all
