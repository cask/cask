#!/bin/sh
# Install cask

CASK_DIR_ABBREV="\$HOME/.cask"
eval CASK_DIR=$CASK_DIR_ABBREV
CASK_REPO="https://github.com/cask/cask.git"

if [ -d "$CASK_DIR" ]; then
  echo "go: Directory '$CASK_DIR_ABBREV' exists or Cask is already installed"

  exit 1
fi

echo "go: Installing cask..."
echo

git clone "$CASK_REPO" "$CASK_DIR" || {
  echo "go: Cask could not be installed. Try again later"
  rm -rf "$CASK_DIR"

  exit 1
}

echo
echo "go: Installation was successful! Now, add the cask binary to your PATH."
echo "go:   export PATH=\"$CASK_DIR_ABBREV/bin:\$PATH\""
