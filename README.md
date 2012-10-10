# Carton

![Carton](https://raw.github.com/rejeep/carton/master/carton.png)

Carton for Emacs is what Bundler is to Ruby. It aims to make ELPA
dependency management in Emacs painless (as painless as it can
be). This includes both your local Emacs installation and Emacs
package development.

## Installation

To automatically install Carton, run this command:

    curl -fsSkL https://raw.github.com/rejeep/carton/master/go | sh

You can also clone the repository.

    $ git clone https://github.com/rejeep/carton.git
    
Don't forget to add Carton's bin to your `PATH`.
    
    $ export PATH="/path/to/code/carton/bin:$PATH"
    

## Usage

Create a file called `Carton` in your project root and specify
dependencies. To install all dependencies, run:

    $ carton [install]

This will create a directory called`elpa`, containing all dependencies.

### Local Emacs installation

Add this to your `.emacs` file.

    (require 'package)
    (package-initialize)

That's it!

### Package development

To create a `-pkg.el` file, run:

    $ carton package

To run some Emacs Lisp code with ELPA load paths all set up for you, use:

    $ carton exec [COMMAND]

Example:

    $ carton exec make test

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

### development

Set scope to development dependencies.

    (development [DEPENDENCIES])

Example:

    (development
     (depends-on "ecukes" "0.2.1")
     (depends-on "espuds" "0.1.0"))

## Example

### Local Emacs installation

    (source "melpa" http://melpa.milkbox.net/packages/")

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

## I still don't get it, give me some real examples

These are some projects using Carton:

* <https://github.com/rejeep/emacs>
* <https://github.com/rejeep/wrap-region>
* <https://github.com/rejeep/espuds>
* <https://github.com/rejeep/enclose>
* <https://github.com/rejeep/ruby-end>
* <https://github.com/rejeep/ruby-tools>
* <https://github.com/rejeep/drag-stuff>
* <https://github.com/rejeep/html-script-src>
* <https://github.com/rejeep/citrus-mode>
* <https://github.com/rejeep/ansi>

## Contribution

Be sure to!

Run the tests with:

    $ make
