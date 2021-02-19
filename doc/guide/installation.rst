==============
 Installation
==============

This document guides you through the installation of Cask.

Prerequisites
=============

Cask requires GNU Emacs 24 and Python 3.6 or later on a Unix system.  It will
not work with Emacs 23 and below, or with other flavours of Emacs, e.g. XEmacs.

.. warning::

   Windows support for Cask requires additional work (see
   :ref:`windows setup`).

Manual installation
===================

To install Cask, run the following command:

.. code-block:: console

   $ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

You can also clone the repository explicitly:

.. code-block:: console

   $ git clone https://github.com/cask/cask.git ~/.cask

To upgrade a manual installation, use:

.. code-block:: console

   $ cask upgrade-cask

Package managers
================

Cask is available in Homebrew_, so OS X users can just use:

.. code-block:: console

   $ brew install cask

.. _Homebrew: http://brew.sh/

Setup
=====

Add Cask to your `$PATH`:

.. code-block:: bash

   export PATH="$HOME/.cask/bin:$PATH"

.. _windows setup:

Windows Installation and Setup
==============================

Cask requires the following additional steps to run under Windows.

Both :program:`emacs` and :program:`python` need to be added to your
:envvar:`%PATH%`.

Assuming that python is installed to the default location
(:file:`c:\Python36`) and emacs is under :file:`c:\bin\emacs`.

By Command Line
---------------

.. code-block:: bat

   > setx PATH "%PATH%;c:\Python36\"
   > setx PATH "%PATH%;c:\bin\emacs\bin"
   > setx PATH "%PATH%;%userprofile%\.cask\bin"

By GUI
------

1. Use :kbd:`Win+Pause` to open System Properties.

2. Under Windows 7 or newer, click on :guilabel:`Advanced system settings`.
   
   Under Windows XP, click on the :guilabel:`Advanced` tab.

3. Click on :guilabel:`Environment Variables...`.

4. Under System Variables find :envvar:`Path` then choose to :guilabel:`Edit...`.
   
   At the end of the listed path, append (include the first ``;`` only if not
   already present)::
     
     ;C:\Python36\;C:\bin\emacs\bin

   If you do not have administrative rights to the machine, add
   the above to the User Variables :envvar:`Path`.

5. Under User Variables find :envvar:`Path`, and edit.  If not present select
   :guilabel:`New...` and name it :envvar:`Path`.

   Append or insert (add a ``;`` at the beginning if :envvar:`Path` exists)::
     
     %userprofile%\.cask\bin
