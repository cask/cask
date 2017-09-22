=================
 Troubleshooting
=================

Error when running a Cask command
=================================

If you run a Cask command and get an error, there are a few things you can try
yourself:

- Make sure that you have the latest Cask version.  You can determine the
  current Cask version with `cask --version`.
- Upgrade Cask with `cask upgrade-cask`.

  .. warning::

     Use `cask upgrade-cask` even if you installed Cask with `git pull`.  `cask
     upgrade-cask` will update the internal dependencies of Cask as well.

- If the error persists, remove Cask's internal dependencies, located at
  :file:`~/.emacs.d/.cask/{emacs-version}/bootstrap`, where :samp:`emacs-version`
  is the version of Emacs you are using.

  Remove that directory and try again.  Cask will automatically download all
  internal dependencies again.

If Cask still does not work, please `report an issue`_ to the issue tracker.
Please include Cask output with the :std:option:`cask --verbose` and
:std:option:`cask --debug` options set, to give us as much information as
possible.

.. _report an issue: https://github.com/cask/cask/issues/new
