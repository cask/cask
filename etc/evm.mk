## Setup
#
# 1. Add `include PATH/TO/THIS.mk` in your Makefile.
# 2. Define test target named `test`.

## Usage
#
# 1. Run test against multiple Emacs installation under `~/.evm` using
#    `make -j4 multi-test` where `-j4` means to use 4 processes.
# 2. If you don't have EVM or want to pass list of Emacs executables
#    explicitly, you can use
#    `make -j4 EMACS_LIST='emacs24 emacs-snapshot' multi-test`.

EVM = ${HOME}/.evm
EMACS_LIST = $(wildcard ${EVM}/installations/*/bin/emacs)
JOBS = $(addprefix job-,${EMACS_LIST})

.PHONY: multi-test ${JOBS}

multi-test: ${JOBS}

${JOBS}: job-%:
	${MAKE} EMACS=$* test
