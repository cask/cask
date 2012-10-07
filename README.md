# Carton

Think of Carton for Emacs as what Bundler is for Ruby. Carton aims to
solve make dependency management in Emacs easy. This includes both
your local Emacs installation and Emacs package development. Carton is
based on ELPA, which now is a part of Emacs.

## Installation

Clone this repo on your computer and add the `carton` binary to your `PATH`.

$ cd /path/to/code
$ git clone https://github.com/rejeep/carton.git
$ export PATH="/path/to/code/carton/bin:$PATH"

## DSL

### source

Add an ELPA mirror.

    (source NAME URL)
    
Example:
    
    (source "marmalade" "http://marmalade-repo.org/packages")

### package

Define this package. Used only for package development.

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
     (depends-on "ecukes "0.2.1"))

## Usage

Create a file called `Carton` in your project root and specify dependencies.

### Local Emacs installation

Install all packages in your carton file:

    $ carton
    
There should now be a folder called `elpa`, with all specified packages.

### Package development

Install all development dependencies in your carton file:

    $ carton
    
There should now be a folder called `elpa`, with all specified dependencies.

To create the `-pkg.el` file, run:

    $ carton package

## Example

### Local Emacs installation

    (source "marmalade" "http://marmalade-repo.org/packages")
     
    (depends-on "magit")
    (depends-on "drag-stuff")
    (depends-on "wrap-region")

### Package development

    (source "marmalade" "http://marmalade-repo.org/packages")
     
    (package "ecukes" "0.2.1" "Cucumber for Emacs.")
     
    (depends-on "ansi")

    (development
     (depends-on "el-mock")
     (depends-on "ert"))
