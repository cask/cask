all:
	carton exec ${EMACS} --script ./test/carton-test -Q -nw
