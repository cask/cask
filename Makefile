all:
	carton exec ${EMACS} --script ./test/carton-testrunner -Q -nw
