EMACS ?= emacs
TAGS ?=
CASK ?= cask
ECUKES = ecukes
ECUKES_ARGS = --script features ${TAGS}
SERVER = ${CASK} exec ${EMACS} --load server/app.el -Q

PKG_DIR := $(shell ${CASK} package-directory)

export EMACS
export CASK

all: ecukes

ecukes: elpa
	${CASK} exec ${ECUKES} ${ECUKES_ARGS}

start-server: elpa tmp
	${SERVER} --batch > tmp/server.log 2>&1 &

stop-server:
	kill $$(cat tmp/server.pid)

server: elpa
	${SERVER} -nw

elpa: ${PKG_DIR}
${PKG_DIR}: Cask
	${CASK} install
	touch $@
# NOTE: `touch` is called here since `cask install` does not update
# timestamp of ${PKG_DIR} directory.

tmp:
	mkdir $@

clean:
	rm -rf ${PKG_DIR}

.PHONY: elpa server ecukes all
