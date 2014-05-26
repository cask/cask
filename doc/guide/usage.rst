=======
 Usage
=======

.. default-domain:: std

This document explains how to use Cask, and provides a reference of its commands
and options.

Quickstart
==========

Start by creating a file named :file:`Cask` in the project root.  Use
:program:`cask init` command to create a :file:`Cask`\ -file automatically,
containing boilerplate code:

.. code-block:: console

   $ cask init [--dev]

Use :option:`cask init --dev`, if the project is for package development!

If you are using Cask for your Emacs configuration, add this to your
:file:`~/.emacs.d/init.el` file:

.. code-block:: cl

   (require 'cask "~/.cask/cask.el")
   (cask-initialize)

To install all dependencies, run:

.. code-block:: console

   $ cask install

This will create a directory called `.cask` and install all dependencies into
it.

By default, packages are installed for the default Emacs, i.e. the one behind
the `emacs` command.  To pick a different Emacs, set the environment variable
:envvar:`EMACS` to the command name or executable path of the Emacs to use:

.. code-block:: console

   $ EMACS="emacs24.1" cask command

Commands and options
====================

The general syntax of the :program:`cask` program is as follows::


   cask [GLOBAL-OPTIONS] [COMMAND] [COMMAND-OPTIONS] [COMMAND-ARGUMENTS]

.. _cask exec:

cask exec
---------

.. program:: cask exec

::

   cask [GLOBAL-OPTIONS] exec [COMMAND] [ARGUMENTS ...]

Execute the :var:`command` with the given :var:`arguments`, with a proper
`$PATH` (see :ref:`cask path`) and `$EMACSLOADPATH` (see :ref:`cask load-path`).

.. _cask help:

cask help
---------

.. program:: cask help

::

   cask [GLOBAL-OPTIONS] help [COMMAND]

Show help about Cask, or a given ``COMMAND``.

.. _cask info:

cask info
---------

.. program:: cask info

::

   cask [GLOBAL-OPTIONS] info

Show information about the project, such as name, description and version.

.. _cask init:

cask init
---------

.. program:: cask init

::

   cask [GLOBAL-OPTIONS] init [--dev]

Create new :file:`Cask`\ -file in the current directory.

If the project is for package development, use the :option:`--dev` option:

.. option:: --dev

   Add additional code to the :file:`Cask` file, which is specific to Emacs Lisp
   packages.

.. _cask install:

cask install
------------

.. program:: cask install

::

   cask [GLOBAL-OPTIONS] [install]

Install all dependencies of the project.  This is the default command.

.. _cask list:

cask list
---------

.. program:: cask list

::

   cask [GLOBAL-OPTIONS] list

List all runtime and development dependencies.

.. _cask load-path:

cask load-path
--------------

.. program:: cask load-path

::

   cask [GLOBAL-OPTIONS] load-path

Print the load path containing the dependencies of the current project, in
proper format for the :envvar:`EMACSLOADPATH` environment variable.

:ref:`cask exec` automatically runs its commands with the proper load-path.

.. _cask outdated:

cask outdated
-------------

.. program:: cask outdated

::

   cask [GLOBAL-OPTIONS] outdated

Show all outdated dependencies.

.. _cask pkg-file:

cask pkg-file
-------------

.. program:: cask pkg-file

::

   cask [GLOBAL-OPTIONS] pkg-file

Write a package descriptor file to :file:`{project}-pkg.el` in the project root.
``project`` is the project name, as declared in the :file:`Cask` file.  See
:infonode:`(elisp)Multi-file Packages` for details.

.. _cask package-directory:

cask package-directory
----------------------

.. program:: cask package-directory

::

   cask [GLOBAL-OPTIONS] package-directory

Print path to package directory, where all dependencies are installed.
Currently, this is :file:`.cask/{emacs-version}/elpa`), where ``emacs-version``
is the value of the :el:variable:`emacs-version` variable in Emacs.

.. _cask path:

cask path
---------

.. program:: cask path

::

   cask [GLOBAL-OPTIONS] path

Print the :envvar:`PATH` environment variable of this project.

The :envvar:`PATH` of a project contains the binary directories of all
dependencies, prepended to the :envvar:`PATH` inherited from the current shell.
The binary directory of a package is the ``bin/`` subdirectory of the package.

:ref:`cask exec` uses the :envvar:`PATH` returned by this command when running
programs.

.. _cask update:

cask update
-----------

.. program:: cask update

::

   cask [GLOBAL-OPTIONS] update

Update all dependencies installed in the project.

.. _cask upgrade-cask:

cask upgrade-cask
-----------------

.. program:: cask upgrade-cask

::

   cask [GLOBAL-OPTIONS] upgrade-cask

Upgrade Cask and all its dependencies.

.. _cask version:

cask version
------------

.. program:: cask version

::

   cask [GLOBAL-OPTIONS] version

Print version of the current package.

.. _cask files:

cask files
----------

.. program:: cask files

::

   cask [GLOBAL-OPTIONS] files

Print the list of all package files.

.. _cask build:

cask build
----------

.. program:: cask build

::

   cask [GLOBAL-OPTIONS] build

Byte compile all Emacs Lisp files in the package.  The resulting byte code is
written to the original path, with the extension replaced by ``.elc``.

.. _cask clean-elc:

cask clean-elc
--------------

.. program:: cask clean-elc

::

   cask [GLOBAL-OPTIONS] clean-elc

Remove byte compiled files generated by :ref:`cask build`.

.. _cask link:

cask link
---------

.. program:: cask link

::

   cask [GLOBAL-OPTIONS] link PACKAGE SOURCE
   cask [GLOBAL-OPTIONS] link list
   cask [GLOBAL-OPTIONS] link delete PACKAGE

Handle package links.

:varcode:`cask link {package} {source}` links the given :var:`source` directory
into the package directory of this project, under the given :var:`package` name.

``cask link list`` lists all links, and :varcode:`cask link delete {package}`
deletes the link for the given :var:`package`.

.. _cask package:

cask package
------------

.. program:: cask package

::

   cask [GLOBAL-OPTIONS] package [DISTDIR]

Build a package artefact, and put it into the given :var:`DISTDIR`, defaulting
to :file:`dist/`.

For single-file packages, this command merely copies the corresponding file to
``DISTDIR``, under the correct filename :file:`{package}-{version}.el`.

For multi-file packages, this command creates a TAR archive containing the
package, as :file:`{package}-{version}.tar`.  The TAR archive contains an
appropriate package descriptor as generated by :ref:`cask pkg-file`.

If the :el:function:`files` of the package contain `.texinfo` files and if
:program:`makeinfo` is available, these are compiled to Info before inclusion in
the package, to allow for online reading of the manual in Emacs.

Global options
--------------

.. program:: cask

The following options are available on all Cask commands:

.. option:: --proxy <proxy>

   Set Emacs proxy for HTTP and HTTPS:

   .. code-block:: console

      $ cask --proxy "localhost:8888" install

.. option:: --http-proxy <proxy>

   Set Emacs proxy for HTTP only.

.. option:: --https-proxy <proxy>

   Set Emacs proxy for HTTPS only.

.. option:: --no-proxy <pattern>

   Do not use a proxy for any URL matching :var:`pattern`.

   :var:`pattern` is an Emacs regular expression.

.. option:: --version

   Print Cask's version.

.. option:: --debug

   Enable debug information.

.. option:: --path <directory>

   Use :file:`{directory}/Cask` instead of the :file:`Cask` file in the current
   directory.

.. option:: --verbose

   Show all output from `package.el`.

Environment variables
=====================

.. envvar:: EMACSLOADPATH

   The load path for Emacs, see :infonode:`(elisp)Library Search`.

.. envvar:: EMACS

   The command name or executable path of Emacs.  Cask will use this Emacs in
   its commands, i.e. byte-compile files with this Emacs, install packages for
   this Emacs, and run commands from packages installed for this Emacs.

   If empty, Cask tries to find a reasonable default.  On OS X, Cask tries the
   following Emacsen, in this order:

   - :file:`~/Applications/Emacs.app`
   - :file:`/Applications/Emacs.app`
   - :file:`/usr/local/bin`
   - ``emacs``

   On other Unix variants, e.g. Linux, Cask will simply use ``emacs``.
