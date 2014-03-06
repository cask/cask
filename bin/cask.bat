@ECHO OFF
IF NOT "%~f0" == "~f0" GOTO :WinNT
ECHO.This version of Python has not been built with support for Windows 95/98/Me.
GOTO :EOF
:WinNT
@python "%~dpn0" %*