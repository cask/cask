CASK ?= cask
EMACS ?= emacs
SERVANT ?= servant

SERVANT_DIR = servant
SERVANT_TMP_DIR = $(SERVANT_DIR)/tmp
SERVANT_PACKAGES_DIR = $(SERVANT_DIR)/packages
SERVANT_NEW_PACKAGES_DIR = $(SERVANT_DIR)/new-packages
SERVANT_ARCHIVE_CONTENTS = $(SERVANT_PACKAGES_DIR)/archive-contents
SERVANT_NEW_ARCHIVE_CONTENTS = $(SERVANT_NEW_PACKAGES_DIR)/archive-contents

FIXTURES_DIR = fixtures


all: test

test: unit ecukes

unit:
	$(CASK) exec ert-runner

ecukes:
	$(CASK) exec ecukes

start-server: $(SERVANT_DIR)
	$(CASK) exec $(SERVANT) start > $(SERVANT_TMP_DIR)/servant.log 2>&1 &

stop-server:
	kill $$(cat $(SERVANT_TMP_DIR)/servant.pid)

$(SERVANT_DIR): clean
$(SERVANT_DIR): $(SERVANT_TMP_DIR)
$(SERVANT_DIR): $(SERVANT_PACKAGES_DIR)/archive-contents
$(SERVANT_DIR): $(SERVANT_NEW_PACKAGES_DIR)/archive-contents

$(SERVANT_TMP_DIR):
	@mkdir -p $@

$(SERVANT_PACKAGES_DIR):
	@mkdir -p $@

$(SERVANT_NEW_PACKAGES_DIR):
	@mkdir -p $@

$(SERVANT_PACKAGES_DIR)/package-a-0.0.1.el: $(SERVANT_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/package-a-0.0.1/package-a.el $@

$(SERVANT_PACKAGES_DIR)/package-b-0.0.1.el: $(SERVANT_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/package-b-0.0.1/package-b.el $@

$(SERVANT_PACKAGES_DIR)/package-c-0.0.1.tar: $(SERVANT_PACKAGES_DIR)
	tar -cvf $@ -C $(FIXTURES_DIR) package-c-0.0.1

$(SERVANT_PACKAGES_DIR)/package-d-0.0.1.tar: $(SERVANT_PACKAGES_DIR)
	tar -cvf $@ -C $(FIXTURES_DIR) package-d-0.0.1

$(SERVANT_PACKAGES_DIR)/package-e-0.0.1.tar: $(SERVANT_PACKAGES_DIR)
	tar -cvf $@ -C $(FIXTURES_DIR) package-e-0.0.1

$(SERVANT_PACKAGES_DIR)/package-f-0.0.1.el: $(SERVANT_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/package-f-0.0.1/package-f.el $@

$(SERVANT_NEW_PACKAGES_DIR)/package-a-0.0.2.el: $(SERVANT_NEW_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/package-a-0.0.2/package-a.el $@

$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-a-0.0.1.el
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-b-0.0.1.el
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-c-0.0.1.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-d-0.0.1.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-e-0.0.1.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-f-0.0.1.el
	$(CASK) exec $(SERVANT) index --packages-path $(SERVANT_PACKAGES_DIR)

$(SERVANT_NEW_ARCHIVE_CONTENTS): $(SERVANT_NEW_PACKAGES_DIR)/package-a-0.0.2.el
	$(CASK) exec $(SERVANT) index --packages-path $(SERVANT_NEW_PACKAGES_DIR)

clean:
	rm -rf $(SERVANT_DIR)

.PHONY: start-server stop-server unit ecukes test all clean
