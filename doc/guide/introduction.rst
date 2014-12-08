==========================
 Introduction — Why Cask?
==========================

Cask is an Emacs Lisp project management tool, similar to Maven or Leiningen.  It
aims to control and automate the entire life cycle of an Emacs Lisp package,
including dependency management, packaging, distribution and testing.

This document provides a motivation for using Cask in your Emacs Lisp packages,
or in your personal Emacs configuration.

Package development
===================

So, why should your Emacs Lisp project use Cask? Do you know why:

* Ruby projects have a `gemspec` file?
* Node.js projects have a `package.json` file?
* Clojure projects have a `project.clj` file?
* Emacs Lisp projects have a `Cask` file?

Actually, let us rephrase the last statement.

* Some Emacs Lisp projects have a `Cask` file?

No, let's try that again.

* Some Emacs Lisp projects do not have a `Cask` file?

We will argue that some Emacs Lisp projects may not benefit directly from using
Cask. Those are the projects that:

* Do not have any dependencies
* Do not have any tests
* Do not care about consistency
* Do not care about compiler warnings
* Do not want to make it easy for contributors

So all in all, projects that are not worth using.

Emacs package development has improved drastically during the last couple of
years. From single Emacs Lisp files uploaded to the Emacs Wiki, to high quality
packages, using VCS, that are tested, installable via a package manager and
more.

But there's one thing still missing and that is consistency. Note that *every*
Ruby project has a `gemspec` file, *every* Node.js project has a `package.json`
file and *every* Clojure project has a `project.clj` file.

In those environments, projects are structured, tested, packaged, compiled,
released in the same way. If you find a new project and want to find out what
dependencies it has, you will know exactly where to look. If you want to find
the test for a specific feature, you know exactly where to look.

For Emacs Lisp projects using Cask, this is true as well.

So, even if you feel that your Emacs Lisp project does not have direct benefit
of using Cask, please do so any way.  If not for you, do it for other Emacs Lisp
developers.

Emacs configuration
===================

If you look at the majority of Emacs configurations out there, you
will see a few different types setups. These are the major ones:

Using package.el directly
-------------------------

It usually looks something like this:

.. code-block:: cl

   (require 'package)
   (package-initialize)
   (mapc
    (lambda (package)
      (unless (package-installed-p package)
        (package-install package)))
    '(s f dash flycheck prodigy ...))

I did something like this in my configuration once as well, but I no longer have
to, because Cask exists.

Submodules
----------

I have over 60 packages in my Emacs configuration. Can you imagine how much work
it would require to keep all of those up to date?

Bundled packages
----------------

This has the same "keeping up to date" issue as the submodules approach. But
it's even worse. Storing dependencies as part of the repository is madness. I
shouldn't have to explain why.

Cask
----

This is obviously what we want. All it is, is a single file that declares a list
of dependencies. You know where to look if you want to find out what
dependencies a configuration has and it's easy to keep packages up to date.
