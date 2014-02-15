CASK ?= cask
EMACS ?= emacs

SERVANT_DIR = 'servant'
SERVANT_TMP_DIR = $(SERVANT_DIR)/'tmp'

all: test

test: unit ecukes

unit:
	$(CASK) exec ert-runner

ecukes:
	$(CASK) exec ecukes

index:
	$(CASK) exec servant index --packages-path $(SERVANT_DIR)/packages
	$(CASK) exec servant index --packages-path $(SERVANT_DIR)/new-packages

start-server: $(SERVANT_DIR) index
	$(SERVANT_DIR)/server

stop-server:
	kill $$(cat $(SERVANT_TMP_DIR)/servant.pid)

$(SERVANT_DIR):
	@mkdir -p $(SERVANT_DIR)
	@mkdir -p $(SERVANT_TMP_DIR)

.PHONY: start-server stop-server unit ecukes test all index
