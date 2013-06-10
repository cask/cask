# Use ?= to respect environment variable (if set):
EMACS ?= emacs
CARTON = ${PWD}/bin/carton
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)
ECUKES_ARGS = --script features
SERVER = ${CARTON} exec ${EMACS} --load server/app.el -Q

export EMACS
export CARTON

all: unit ecukes

unit: elpa
	./test/carton-test

ecukes: elpa
	${CARTON} exec ${ECUKES} ${ECUKES_ARGS}

start-server: elpa tmp
	${SERVER} --batch > tmp/server.log 2>&1 &

stop-server:
	kill $$(cat tmp/server.pid)

server: elpa
	${SERVER} -nw

elpa: Carton
	${CARTON} install
	touch $@
# NOTE: `touch` is called here since `carton install` does not update
# timestamp of `elpa` directory.

tmp:
	mkdir $@

clean:
	rm -rf elpa

smoke:
	cd test/smoke/ && ${CARTON} install

.PHONY:	server ecukes unit all
