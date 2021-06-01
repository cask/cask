export CASK ?= bin/cask
export EMACS ?= emacs
export CASK_DIR := $(shell $(CASK) package-directory)
SERVANT ?= servant
SPHINX-BUILD = sphinx-build
SPHINXFLAGS =

SERVANT_DIR = servant
SERVANT_TMP_DIR = $(SERVANT_DIR)/tmp
SERVANT_PACKAGES_DIR = $(SERVANT_DIR)/packages
SERVANT_NEW_PACKAGES_DIR = $(SERVANT_DIR)/new-packages
SERVANT_ARCHIVE_CONTENTS = $(SERVANT_PACKAGES_DIR)/archive-contents
SERVANT_NEW_ARCHIVE_CONTENTS = $(SERVANT_NEW_PACKAGES_DIR)/archive-contents

DOCBUILDDIR=build/doc
DOCTREEDIR=$(DOCBUILDDIR)/doctrees

FIXTURES_DIR = fixtures

.PHONY: all
all: test

.PHONY: test
test: compile spaces unit ecukes

.PHONY: compile
compile: cask
	if 1>/dev/null expr $$(2>&1 $(EMACS) -Q --batch --eval '(let ((inhibit-message t)) (princ emacs-major-version (function external-debugging-output)))') ">" 24 ; then \
	  ! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 \
	     | egrep -a "(Warning|Error):") ; \
	  (ret=$$? ; $(CASK) clean-elc && exit $$ret) \
	else echo Not linting emacs24 ; fi

.PHONY: spaces
spaces:
	bash -c "trap 'ret=$$? ; trap \"\" EXIT; cd .. ; rm -rf \"cask cask cask\" ; exit $$ret' EXIT ; mkdir -p \"cask cask cask/bin\" ; cp -p $(CASK) \"cask cask cask/bin\" ; cd \"cask cask cask\" ; EMACS=true bash -eux $(CASK)"
	bash -c "trap 'ret=$$? ; trap \"\" EXIT; cd .. ; rm -rf \"cask cask cask\" ; exit $$ret' EXIT ; mkdir -p \"cask cask cask/bin\" ; cp -p $(CASK) \"cask cask cask/bin\" ; cd \"cask cask cask\" ; WHICH=true EMACS=true bash -eux $(CASK)"

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: unit
unit:
	$(MAKE) start-server
	bash -c "trap 'trap \"\" EXIT ; $(MAKE) -C $(CURDIR) stop-server' EXIT ; $(CASK) exec ert-runner -L ./test -l test/test-helper.el test/cask-*test.el | tee /tmp/cask.unit.out"
	! (grep -q "unexpected results" /tmp/cask.unit.out)

.PHONY: ecukes
ecukes:
	$(MAKE) start-server
	bash -c "trap 'ret=$$? ; trap \"\" EXIT ; $(MAKE) -C $(CURDIR) stop-server; exit $$ret' EXIT ; $(CASK) exec ecukes --reporter magnars"

.PHONY: doc
doc: html

.PHONY: html
html:
	$(SPHINX-BUILD) -b html -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/html

.PHONY: linkcheck
linkcheck :
	$(SPHINX-BUILD) -b linkcheck -d $(DOCTREEDIR) $(SPHINXFLAGS) doc $(DOCBUILDDIR)/linkcheck

.SILENT: start-server
.PHONY: start-server
start-server: cask $(SERVANT_DIR)
	$(CASK) exec $(SERVANT) start > $(SERVANT_TMP_DIR)/servant.log 2>&1 &

.SILENT: stop-server
.PHONY: stop-server
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

$(SERVANT_PACKAGES_DIR)/project-99.0.1.el: $(SERVANT_PACKAGES_DIR)
	cp $(FIXTURES_DIR)/project-99.0.1/project.el $@

$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-a-0.0.1.el
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-b-0.0.1.el
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-c-0.0.1.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-d-0.0.1.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-e-0.0.1.tar
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/package-f-0.0.1.el
$(SERVANT_ARCHIVE_CONTENTS): $(SERVANT_PACKAGES_DIR)/project-99.0.1.el
	$(CASK) exec $(SERVANT) index --packages-path $(SERVANT_PACKAGES_DIR)

$(SERVANT_NEW_ARCHIVE_CONTENTS): $(SERVANT_NEW_PACKAGES_DIR)/package-a-0.0.2.el
	$(CASK) exec $(SERVANT) index --packages-path $(SERVANT_NEW_PACKAGES_DIR)

.PHONY: clean
clean:
	rm -rf $(SERVANT_DIR)
	rm -rf $(DOCBUILDDIR)

README.makefile: README.org
	$(EMACS) -Q --batch -l ob-tangle --eval "(org-babel-tangle-file \"$(<)\" nil \"makefile\")"

.github/workflows/readme.yml: README.org
	$(EMACS) -Q --batch -l ob-tangle \
	  --eval "(add-hook (quote org-babel-tangle-body-hook) \
	            (lambda () \
	              (goto-char (point-min)) \
                      (insert \"name: readme\n\") \
	              (insert \"on:\n\") \
	              (insert \"  pull_request:\n\") \
	              (insert \"  push:\n\") \
	              (insert \"    branches-ignore:\n\") \
	              (insert \"    - 'master'\n\") \
	              (insert \"    - 'main'\n\") \
	              (goto-char (point-max)) \
	              (insert \"\n\") \
	              (insert \"      - run: make -f README.makefile test\n\")))" \
	  --eval "(org-babel-tangle-file \"$(<)\" nil \"yaml\")"

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	@bash -c "trap 'ret=$$? ; trap \"\" EXIT; mv -f Cask.orig Cask ; exit $$ret' EXIT ; cp Cask Cask.orig ; 1>/dev/null expr $$(2>&1 $(EMACS) -Q --batch --eval '(let ((inhibit-message t)) (princ emacs-major-version (function external-debugging-output)))') '<=' 24 && sed -i '/package-build-legacy/d' ./Cask ; $(CASK) package"

.PHONY: install
install: dist
	$(CASK) eval "(progn \
	  (add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\")) \
	  (package-refresh-contents) \
	  (package-install-file \"dist/cask-$(shell $(CASK) version).tar\"))"
	$(eval INSTALLED = $(shell 2>&1 $(EMACS) -Q --batch --eval "(let ((inhibit-message t)) (package-initialize) (princ (directory-file-name (file-name-directory (locate-library \"cask\"))) (function external-debugging-output)))"))
	@if [ -z "$(INSTALLED)" ] ; then \
	  echo ERROR: package-install-file failed ; \
	  false ; \
	fi
	$(eval TARGET = \
	  $(shell if 1>/dev/null which systemd-path ; then \
	            echo "$$(systemd-path user-binaries)/cask" ; \
	          elif [ ! -z "$(XDG_DATA_HOME)" ] ; then \
	            echo "$(XDG_DATA_HOME)/../bin/cask" ; \
	          elif [ -d "$(HOME)/.local/bin" ] ; then \
	            echo "$(HOME)/.local/bin/cask" ; \
	          elif [ -d "$(HOME)/bin" ] ; then \
	            echo "$(HOME)/bin/cask" ; \
	          fi))
	@if [ -z "$(TARGET)" ] ; then \
	  echo ERROR: Do not know where to install cask ; \
	  [ ! -z "$${GITHUB_WORKFLOW:-}" ] ; \
	elif [ -L "$(TARGET)" ] ; then \
	  rm -f "$(TARGET)" ; \
	  ln -s $(INSTALLED)/bin/cask $(TARGET) ; \
	elif [ -e "$(TARGET)" ] ; then \
	  echo ERROR: Cannot install over $(TARGET) ; \
	  false ; \
	elif [ ! -d "$$(dirname $(TARGET))" ]; then \
	  echo ERROR: "$$(dirname $(TARGET))" does not exist ; \
	  [ ! -z "$${GITHUB_WORKFLOW:-}" ] ; \
	else \
	  ln -s $(INSTALLED)/bin/cask $(TARGET) ; \
	fi
