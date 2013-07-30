#!/bin/sh

CASK_DIR="$HOME/.cask"

if [ -d "$CASK_DIR" ]; then
  echo "Cask is already installed at '$CASK_DIR'"

  exit 1
else
  git clone https://github.com/rejeep/cask.el.git $CASK_DIR 2> /dev/null

  echo "Successfully installed Cask! Now, add the cask binary to your PATH."
  echo '  export PATH="'${CASK_DIR}'/bin:$PATH"'

  exit 0
fi
