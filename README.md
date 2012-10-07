# Carton

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
    
This will create a `.cartons` directory, containing all dependencies.

If you add a package to your `Carton` file, you can install only that
package with:

    $ carton update

### Local Emacs installation

When using carton in your local Emacs installation, make sure
`carton.el` is in your path, and add this to your `.emacs` file.

    (require 'carton)
    (carton-setup)
    (package-initialize)

### Package development

To create a `-pkg.el` file, run:

    $ carton package

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
