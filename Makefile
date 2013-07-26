# Use ?= to respect environment variable (if set):
EMACS ?= emacs
TAGS ?= '--tags ~@only-in-emacs-23'
CARTON ?= ${PWD}/bin/carton
ECUKES = $(shell find ${PKG_DIR}/ecukes-*/ecukes | tail -1)
ECUKES_ARGS = --script features ${TAGS}
SERVER = ${CARTON} exec --debug ${EMACS} --load server/app.el -Q

PKG_DIR = $(shell ${EMACS} -Q --batch --eval \
'(princ (format ".carton/%s/elpa" emacs-version))')

export EMACS
export CARTON

all: ecukes

ecukes: elpa
	${CARTON} exec ${ECUKES} ${ECUKES_ARGS}

start-server: elpa tmp
	${SERVER} --batch > tmp/server.log 2>&1 &

stop-server:
	kill $$(cat tmp/server.pid)

server: elpa
	${SERVER} -nw

elpa: ${PKG_DIR}
${PKG_DIR}: Carton
	${CARTON} install
	touch $@
# NOTE: `touch` is called here since `carton install` does not update
# timestamp of ${PKG_DIR} directory.

tmp:
	mkdir $@

clean:
	rm -rf ${PKG_DIR}

.PHONY: elpa server ecukes all
