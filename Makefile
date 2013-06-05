all: unit ecukes

unit:
	./test/carton-test

ecukes:
	carton exec ecukes --script features

server:
	carton exec emacs --load server/app.el -Q -nw

.PHONY:	server ecukes unit all
