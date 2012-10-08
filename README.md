# Carton

![Carton](https://raw.github.com/rejeep/carton/master/carton.png)

Carton for Emacs is what Bundler is to Ruby. It aims to make ELPA
dependency management in Emacs painless (as painless as it can
be). This includes both your local Emacs installation and Emacs
package development.

## Installation

Clone this repository on your computer and add the `bin` directory to
your `PATH`.

    $ cd /path/to/code
    $ git clone https://github.com/rejeep/carton.git
    $ export PATH="/path/to/code/carton/bin:$PATH"

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

## Usage

Create a file called `Carton` in your project root and specify
dependencies. To install all dependencies, run:

    $ carton [install]
    
This will create a `elpa` directory, containing all dependencies.

### Local Emacs installation

_(yes, this is very annying and we would all be very happy if something like this would be included in Emacs by default)_

When using carton for your local Emacs installation, add `carton` to
your `Carton` file:

    (depends-on "carton")

Run the `carton` command to install. Then add this to your `.emacs` file.

    (add-to-list 'load-path (car (file-expand-wildcards "~/.emacs.d/elpa/carton-*")))
    (require 'carton)
    (carton-setup)
    (package-initialize)

### Package development

To create a `-pkg.el` file, run:

    $ carton package
    
To run some Emacs Lisp code with ELPA load paths all set up for you, use:

    $ carton exec [COMMAND]
    
Example:

    $ carton exec make test

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

## Contribution

Be sure to!

Run the tests with:

    $ make
