======
 Cask
======

Cask is a project management tool for Emacs Lisp to automate the package
development cycle; development, dependencies, testing, building, packaging and
more.

Cask can also be used to manage dependencies for your local Emacs configuration.

It's based on a :file:`Cask` file, which identifies an Emacs Lisp package,
provides meta information about the package, and declares its contents and
dependencies.

.. figure:: /images/cask-file.png
   :align: center
   :width: 876
   :height: 403

   Cask's own :file:`Cask` file

.. contents:: Table of Contents
   :local:

.. _user-guide:

User guide
==========

This part of the documentation explains how to use Cask.  We start with a little
introduction on Cask, which provides background information and motivation for
Cask.  Then we guide you through the installation and usage of Cask, and provide
a reference on Cask's domain specific language.  We conclude with some
troubleshooting help.

.. toctree::
   :maxdepth: 2

   guide/introduction
   guide/installation
   guide/usage
   guide/dsl
   guide/troubleshoot

Developer guide
===============

This part of the documentation shows how to write extensions for and packages
based on Cask, and explains how to contribute to Cask.

.. toctree::
   :maxdepth: 2

   dev/api
   dev/contributing

Licensing
=========

Cask is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Cask is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

See :doc:`gpl` or http://www.gnu.org/licenses/ for a copy of the GNU General
Public License.

.. toctree::
   :maxdepth: 1

   gpl

Index
=====

The index provides a sorted list of all symbols, variables and concepts
explained throughout Cask's documentation:

- :ref:`genindex`
