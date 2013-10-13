#!/bin/sh
# Install cask

CASK_DIR="$HOME/.cask"
CASK_REPO="https://github.com/cask/cask.git"

if [ -d "$CASK_DIR" ]; then
  echo "Cask is already installed at '$CASK_DIR'"

  exit 1
fi

git clone -q "$CASK_REPO" "$CASK_DIR"
RET=$?

if [ "$RET" -eq 0 ]; then
  echo "Successfully installed Cask! Now, add the cask binary to your PATH."
  echo "  export PATH=\"$CASK_DIR/bin:\$PATH\""
else
  echo "Cask could not be installed."
fi

exit $RET
