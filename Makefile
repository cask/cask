CARTON = bin/carton
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: unit ecukes

unit:
	./test/carton-test

ecukes:
	${CARTON} exec ${ECUKES} --script features

server:
	${CARTON} exec emacs --load server/app.el -Q -nw

.PHONY:	server ecukes unit all
