EMACS ?= emacs
TAGS ?=
CASK ?= cask
ECUKES = ecukes
ECUKES_ARGS = --script features ${TAGS}
SERVER = ${CASK} exec ${EMACS} -Q --load servant/app.el

PKG_DIR := $(shell ${CASK} package-directory)

export EMACS
export CASK

all: test

test: elpa unit ecukes

ecukes:
	${CASK} exec ${ECUKES} ${ECUKES_ARGS}

start-server: elpa
	mkdir -p servant/tmp
	${SERVER} --batch > servant/tmp/servant.log 2>&1 &

stop-server:
	kill $$(cat servant/tmp/servant.pid)

server: elpa
	${SERVER} -nw

elpa: ${PKG_DIR}
${PKG_DIR}: Cask
	${CASK} install
	touch $@
# NOTE: `touch` is called here since `cask install` does not update
# timestamp of ${PKG_DIR} directory.

clean:
	rm -rf ${PKG_DIR}

unit:
	${CASK} exec ert-runner

.PHONY: elpa server ecukes all unit
