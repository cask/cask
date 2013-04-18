ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: unit ecukes

unit:
	./test/carton-test

ecukes:
	carton exec ${ECUKES} features

server:
	carton exec emacs --load server/app.el -Q -nw

.PHONY:	server ecukes unit all
