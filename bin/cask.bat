@echo off
bash %~dp0cask %*

REM %~dp0 expands to match CASK_BIN_DIRECTORY.
REM %* passes all arguments to "python cask"
