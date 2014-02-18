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

$(SERVANT_DIR): $(SERVANT_TMP_DIR)
$(SERVANT_DIR): $(SERVANT_PACKAGES_DIR)/archive-contents
$(SERVANT_DIR): $(SERVANT_NEW_PACKAGES_DIR)/archive-contents

$(SERVANT_TMP_DIR):
	@mkdir -p $@

$(SERVANT_PACKAGES_DIR):
	@mkdir -p $@

$(SERVANT_NEW_PACKAGES_DIR):
	@mkdir -p $@

$(SERVANT_PACKAGES_DIR)/foo-0.0.1.el: $(SERVANT_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/foo-0.0.1/foo.el $@

$(SERVANT_PACKAGES_DIR)/bar-0.0.2.el: $(SERVANT_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/bar-0.0.2/bar.el $@

$(SERVANT_PACKAGES_DIR)/baz-0.0.3.tar: $(SERVANT_PACKAGES_DIR)
	tar -cvf $@ -C $(FIXTURES_DIR) baz-0.0.3

$(SERVANT_PACKAGES_DIR)/qux-0.0.4.tar: $(SERVANT_PACKAGES_DIR)
	tar -cvf $@ -C $(FIXTURES_DIR) qux-0.0.4

$(SERVANT_PACKAGES_DIR)/hey-0.0.5.tar: $(SERVANT_PACKAGES_DIR)
	tar -cvf $@ -C $(FIXTURES_DIR) hey-0.0.5

$(SERVANT_PACKAGES_DIR)/fux-0.0.6.el: $(SERVANT_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/fux-0.0.6/fux.el $@

$(SERVANT_PACKAGES_DIR)/dep-0.0.7.el: $(SERVANT_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/dep-0.0.7/dep.el $@

$(SERVANT_NEW_PACKAGES_DIR)/foo-0.0.2.el: $(SERVANT_NEW_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/foo-0.0.2/foo.el $@

$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/foo-0.0.1.el
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/bar-0.0.2.el
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/baz-0.0.3.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/qux-0.0.4.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/hey-0.0.5.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/fux-0.0.6.el
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/dep-0.0.7.el
	$(CASK) exec $(SERVANT) index --packages-path $(SERVANT_PACKAGES_DIR)

$(SERVANT_NEW_ARCHIVE_CONTENTS): $(SERVANT_NEW_PACKAGES_DIR)/foo-0.0.2.el
	$(CASK) exec $(SERVANT) index --packages-path $(SERVANT_NEW_PACKAGES_DIR)

clean:
	rm -rf $(SERVANT_DIR)

.PHONY: start-server stop-server unit ecukes test all clean
