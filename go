#!/bin/sh

CARTON_DIR="$HOME/.carton"

if [ -d "$CARTON_DIR" ]; then
  echo "Carton is already installed at '$CARTON_DIR'"

  exit 1
else
  git clone https://github.com/rejeep/carton.git $CARTON_DIR 2> /dev/null

  echo "Successfully installed Carton! Now, add the carton binary to your PATH."
  echo '  export PATH="'${CARTON_DIR}'/bin:$PATH"'

  exit 0
fi
