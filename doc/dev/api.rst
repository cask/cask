==========
 Cask API
==========

.. default-domain:: el

.. require:: cask

This document provides a reference of the public Cask API, which you may use in
your own projects and extensions to Cask.

.. contents:: Table of Contents
   :local:

Cask bundles
============

A bundle represents a specific Cask project.  Essentially, a bundle is a loaded
:file:`Cask` file.

Creating bundles
================

The following functions create bundles.

.. function:: cask-setup
   :auto:

.. function:: cask-initialize
   :auto:

Bundle paths
============

These functions return various paths associated with a bundle:

.. function:: cask-file
   :auto:

.. function:: cask-path
   :auto:

.. function:: cask-load-path
   :auto:

.. function:: cask-exec-path
   :auto:

.. function:: cask-elpa-path
   :auto:

Package metadata of bundles
===========================

These functions give access to the metadata of the package, represented by the
bundle.

.. function:: cask-package-name
   :auto:

.. function:: cask-package-version
   :auto:

.. function:: cask-package-description
   :auto:

Bundle contents
===============

.. function:: cask-files
   :auto:

Bundle dependencies
===================

.. function:: cask-dependencies
   :auto:

.. function:: cask-runtime-dependencies
   :auto:

.. function:: cask-development-dependencies
   :auto:

.. function:: cask-installed-dependencies
   :auto:

.. function:: cask-add-dependency
   :auto:

.. function:: cask-has-dependency
   :auto:

.. function:: cask-find-dependency
   :auto:

.. function:: cask-dependency-path
   :auto:

Dependency links
================

These functions deal with dependency links.

.. seealso::

   :ref:`cask link`

.. function:: cask-links
   :auto:

.. function:: cask-link
   :auto:

.. function:: cask-link-delete
   :auto:

.. function:: cask-linked-p
   :auto:

Dependency sources and package archives
=======================================

These functions let you add and remove dependency sources, i.e., package
archives where to get dependencies from.

.. function:: cask-add-source
   :auto:

.. function:: cask-remove-source
   :auto:

Dependency operations
=====================

These functions provide operations on dependencies, such as updating, or
installing them:

.. function:: cask-install
   :auto:

.. function:: cask-update
   :auto:

.. function:: cask-outdated
   :auto:

Byte compilation
================

These function let you byte compile all Emacs Lisp files in a bundle:

.. function:: cask-build
   :auto:

.. function:: cask-clean-elc
   :auto:

Packaging
=========

These functions create packages and package descriptors:

.. function:: cask-define-package-string
   :auto:

.. function:: cask-define-package-file
   :auto:

.. function:: cask-package
   :auto:

Miscellaneous functions
=======================

.. function:: cask-caskify
   :auto:

.. function:: cask-version
   :auto:
