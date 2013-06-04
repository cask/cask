CARTON = ${PWD}/bin/carton
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: unit ecukes

unit:
	./test/carton-test

ecukes:
	carton exec ${ECUKES} --script features

server:
	carton exec emacs --load server/app.el -Q -nw

smoke:
	cd test/smoke/ && ${CARTON} install

.PHONY:	server ecukes unit all
