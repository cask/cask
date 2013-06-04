# Use ?= to respect environment variable (if set):
EMACS ?= emacs
CARTON = ${PWD}/bin/carton
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

export EMACS
export CARTON

all: unit ecukes

unit: elpa
	./test/carton-test

ecukes: elpa
	${CARTON} exec ${ECUKES} --script features

server: elpa
	${CARTON} exec ${EMACS} --load server/app.el -Q -nw

elpa: Carton
	${CARTON} install
	touch $@
# NOTE: `touch` is called here since `carton install` does not update
# timestamp of `elpa` directory.

clean:
	rm -rf elpa

smoke:
	cd test/smoke/ && ${CARTON} install

.PHONY:	server ecukes unit all
