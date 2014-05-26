==============
 Installation
==============

This document guides you through the installation of Cask.

Prerequisites
=============

Cask requires GNU Emacs 24 and Python 2.6 or later on a Unix system.  It will
not work with Emacs 23 and below, or with other flavours of Emacs, e.g. XEmacs.

.. warning::

   Cask does not currently support Windows.

Manual installation
===================

To install Cask, run the following command:

.. code-block:: console

   $ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

If you do not trust us, `inspect the script first
<https://github.com/cask/cask/blob/master/go>`_.

You can also clone the repository explicitly:

.. code-block:: console

   $ git clone https://github.com/cask/cask.git

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
