# Carton [![Build Status](https://api.travis-ci.org/rejeep/carton.png?branch=master)](http://travis-ci.org/rejeep/carton)

![Carton](https://raw.github.com/rejeep/carton/master/carton.png)

Carton for Emacs is what Bundler is to Ruby. It aims to make ELPA
dependency management in Emacs painless (as painless as it can
be). This includes both your local Emacs installation and Emacs
package development.

[<img src="http://img.youtube.com/vi/gzFxNO_X5yA/0.jpg">](https://www.youtube.com/watch?v=gzFxNO_X5yA)

## Installation

To automatically install Carton, run this command:

    curl -fsSkL https://raw.github.com/rejeep/carton/master/go | sh

You can also clone the repository.

    $ git clone https://github.com/rejeep/carton.git

Don't forget to add Carton's bin to your `PATH`.

    $ export PATH="/path/to/code/carton/bin:$PATH"


## Usage

Create a file called `Carton` in your project root and specify
dependencies:

    $ carton init [--dev]

_(Use `--dev` if the project is for package development)_

To install all dependencies, run:

    $ carton [install]

This will create a directory called `elpa`, containing all dependencies.

To update package version, run:

    $ carton update

To list all dependencies, run:

    $ carton list

### Local Emacs installation

Add this to your `.emacs` file.

    (require 'package)
    (setq package-user-dir
          (locate-user-emacs-file (format ".carton/%s/elpa/" emacs-version)))
    (package-initialize)

That's it!

#### Alternative method

Alternatively, if you install Carton under `$HOME/.carton` using the
automatic installation script, you can add this in your `.emacs` file
instead.

    (require 'carton "~/.carton/carton.el")
    (carton-initialize)

#### Tips

To automatically keep the `Carton` file up to date with what you
install from ELPA, check out <https://github.com/rdallasgray/pallet>.

### Package development

To create a `-pkg.el` file, run:

    $ carton package

To run some Emacs Lisp code with ELPA load paths all set up for you, use:

    $ carton exec [COMMAND]

Example:

    $ carton exec make test

To print info about the current project:

    $ carton info

## DSL

### source

Add an ELPA mirror.

    (source NAME URL)

Example:

    (source "melpa" "http://melpa.milkbox.net/packages/")

### package

Define this package (used only for package development).

    (package NAME VERSION DESCRIPTION)

Example:

    (package "ecukes" "0.2.1" "Cucumber for Emacs.")

### depends-on

Add a runtime dependency.

    (depends-on NAME VERSION)

Example:

    (depends-on "magit" "0.8.1")

### package-file

Define this package and its runtime dependencies from the package headers of a
file (used only for package development).  The name of the file is relative to
the directory containing the `Carton` file.

    (package-file FILENAME)

Example:

    (package-file "foo.el")

### development

Set scope to development dependencies.

    (development [DEPENDENCIES])

Example:

    (development
     (depends-on "ecukes" "0.2.1")
     (depends-on "espuds" "0.1.0"))

## Example

### Local Emacs installation

    (source "melpa" "http://melpa.milkbox.net/packages/")

    (depends-on "magit")
    (depends-on "drag-stuff")
    (depends-on "wrap-region")

### Package development

    (source "melpa" "http://melpa.milkbox.net/packages/")

    (package "ecukes" "0.2.1" "Cucumber for Emacs.")

    (depends-on "ansi")

    (development
     (depends-on "el-mock")
     (depends-on "ert"))

## Completion

To install ZSH completion add the following to your `~/.zshrc`:

    source /path/to/code/carton/etc/carton_completion.zsh

## I still don't get it, give me some real examples

These are some projects using Carton:

* [rejeep/emacs](https://github.com/rejeep/emacs)
* [drag-stuff](https://github.com/rejeep/drag-stuff)
* [emacs-jedi](https://github.com/tkf/emacs-jedi)
* [enclose](https://github.com/rejeep/enclose)
* [espuds](https://github.com/rejeep/espuds)
* [flycheck](https://github.com/lunaryorn/flycheck)
* [html-script-src](https://github.com/rejeep/html-script-src)
* [projectile](https://github.com/bbatsov/projectile)
* [ruby-end](https://github.com/rejeep/ruby-end)
* [ruby-tools](https://github.com/rejeep/ruby-tools)
* [wrap-region](https://github.com/rejeep/wrap-region)
* ...

## Contribution

Be sure to!

For each make command below, prefix with:

    $ EMACS="/path/to/emacs"

For exmaple:

    $ EMACS="/path/to/emacs" make abc

Run the unit tests with:

    $ make unit

To run the Ecukes tests, first start the fake ELPA server:

    $ make server

Then to run the tests:

    $ make ecukes

Run all tests with:

    $ make
