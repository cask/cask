======================
 Contributing to Cask
======================

This document provides guidelines and information on contributing to Cask.

Cask is on GitHub_, and a discussion group is available at
https://groups.google.com/forum/#!forum/cask-dev.

.. _github: https://github.com/cask/cask

Testing
=======

Cask comes with a rich set of test cases.  When fixing bugs or implementing new
features, please add the corresponding test cases as well.

Running tests
-------------

1. `make start-server` to start the fake package server, which is used
   throughout the tests.
2. `make test` to run all tests.  Use `make unit` to only run the unit tests,
   and `make ecukes` to only run the integration tests.
3. Repeat 2. as long as you need.
4. `make stop-server` to stop the fake package server started in 1.

Documentation
=============

Cask includes a comprehensive user guide.  Please try to extend it accordingly
when you implement new features.

The documentation is written in reStructuredText_, using Sphinx_ and
sphinxcontrib-emacs_.  The former is a generic documentation tool, and the
latter extends it with specific support for Emacs Lisp projects.

Setup
-----

To build the documentation locally, you need to go through a little setup first.

Make sure that you have Python 2.7 and virtualenv_ available.  To install
virtualenv_, use the following command:

.. code-block:: console

   $ pip install --user virtualenv

Then add :file:`~/Library/Python/2.7/bin` (on OS X) or :file:`~/.local/bin` (on
other Unix variants) to :envvar:`PATH`.

.. note::

   You probably need to install :program:`pip` first.  It is available in the
   package repositories of most Linux distributions, as ``python-pip`` or
   similar.  If ``pip`` is not available for your Linux distribution, or if you
   are using OS X, please follow the instructions to `install pip`_.

   .. _install pip: https://pip.pypa.io/en/latest/installing.html

Now create a virtualenv for the documentation, and install the requirements:

.. code-block:: console

   $ mkdir -p ~/.virtualenvs
   $ virtualenv -p python2.7 ~/.virtualenvs/cask
   $ pip install -r doc/requirements.txt

Now you are set up to build the documentation.

.. _virtualenv: https://virtualenv.pypa.io/

Building
--------

Now you are ready to build the documentation.

First, switch to the virtualenv and make sure that the requirements are up to
date:

.. code-block:: console

   $ source ~/.virtualenvs/cask/bin/activate
   $ pip install -r doc/requirements.txt

Then you can build the HTML documentation, or verify all links in the
documentation:

.. code-block:: console

   $ make html  # Build HTML documentation to build/doc/html/
   $ make linkcheck  # Check all links in the documentation

.. _reStructuredText: http://docutils.sourceforge.net/rst.html
.. _Sphinx: http://sphinx-doc.org/
.. _sphinxcontrib-emacs: https://sphinxcontrib-emacs.readthedocs.io/

Pull requests
=============

If all tests passes, and the documentation builds, please send us a `pull
request`_ with your changes.

.. note::

   Usually we work on a WIP branch, named :samp:`v{major}.{minor}-wip`.  Your
   pull request should target this branch, if present.  Otherwise just base your
   pull request on `master`.

.. _pull request: https://github.com/cask/cask/pulls

Docker image (Optional)
=======================

If you need clean Emacs environment, you can use Docker image.

Usage
-----

.. code-block:: console

   $ cd cask
   $ docker build -t cask-27.1 -f docker/Dockerfile-27.1 .
   $ docker run -it --rm -v $PWD:/cask cask-27.1 bash
   $ cask

Generate
--------

You can use Dockerfile template file and generate from it.

Just edit ``Dockerfile.mustache`` using mustache grammar and exec ``generate-dockerfile``.

Misc
----

Build images.

.. code-block:: console
    docker build -t cask-24.1 -f docker/Dockerfile-24.1 .
    docker build -t cask-24.2 -f docker/Dockerfile-24.2 .
    docker build -t cask-24.3 -f docker/Dockerfile-24.3 .
    docker build -t cask-24.4 -f docker/Dockerfile-24.4 .
    docker build -t cask-24.5 -f docker/Dockerfile-24.5 .
    docker build -t cask-25.1 -f docker/Dockerfile-25.1 .
    docker build -t cask-25.2 -f docker/Dockerfile-25.2 .
    docker build -t cask-25.3 -f docker/Dockerfile-25.3 .
    docker build -t cask-26.1 -f docker/Dockerfile-26.1 .
    docker build -t cask-26.2 -f docker/Dockerfile-26.2 .
    docker build -t cask-26.3 -f docker/Dockerfile-26.3 .
    docker build -t cask-27.1 -f docker/Dockerfile-27.1 .

Run docker image for Cask-self debugging.

.. code-block:: console
    docker run -it --rm -v $PWD:/cask cask-24.1 bash
    docker run -it --rm -v $PWD:/cask cask-24.2 bash
    docker run -it --rm -v $PWD:/cask cask-24.3 bash
    docker run -it --rm -v $PWD:/cask cask-24.4 bash
    docker run -it --rm -v $PWD:/cask cask-24.5 bash
    docker run -it --rm -v $PWD:/cask cask-25.1 bash
    docker run -it --rm -v $PWD:/cask cask-25.2 bash
    docker run -it --rm -v $PWD:/cask cask-25.3 bash
    docker run -it --rm -v $PWD:/cask cask-26.1 bash
    docker run -it --rm -v $PWD:/cask cask-26.2 bash
    docker run -it --rm -v $PWD:/cask cask-26.3 bash
    docker run -it --rm -v $PWD:/cask cask-27.1 bash

Run docker image for using Cask for wild other packages.
- Assume that cask has been forked to $HOME/dev/forks/cask.
- Assume that the current directory is the folder where the ``Cask`` file.

.. code-block:: console
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-24.1 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-24.2 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-24.3 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-24.4 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-24.5 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-25.1 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-25.2 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-25.3 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-26.1 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-26.2 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-26.3 bash
    docker run -it --rm -v $HOME/dev/forks/cask:/cask -v $PWD:/work -w /work cask-27.1 bash
