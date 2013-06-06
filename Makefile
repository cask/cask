CARTON = bin/carton
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: unit ecukes

unit: elpa
	./test/carton-test

ecukes: elpa
	${CARTON} exec ${ECUKES} --script features

server: elpa
	${CARTON} exec emacs --load server/app.el -Q -nw

elpa:
	${CARTON} install

clean:
	rm -rf elpa

.PHONY:	server ecukes unit all
