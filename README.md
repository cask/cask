# Cask

![Cask](cask.png)


[![Build Status](https://travis-ci.org/cask/cask.svg)](https://travis-ci.org/cask/cask)
[![Coverage Status](https://coveralls.io/repos/cask/cask/badge.svg)](https://coveralls.io/r/cask/cask)
[![MELPA](http://melpa.org/packages/cask-badge.svg)](http://melpa.org/#/cask)
[![MELPA stable](http://stable.melpa.org/packages/cask-badge.svg)](http://stable.melpa.org/#/cask)
[![Tag Version](https://img.shields.io/github/tag/cask/cask.svg)](https://github.com/cask/cask/tags)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Cask is a *project management tool* for Emacs that helps automate the
package development cycle; development, dependencies, testing,
building, packaging and more.

Cask can also be used to manage dependencies for your local Emacs configuration.

## Basic Usage

`cask` is both command suite and a way to describe the dependencies in a `Cask` file.

### Main Command

Here are the main commands to start with:
- `install` : install Cask dependancies
- `exec` : run command in the good cask context
- `init` : to initiate a project
For complete reference, run `cask help` or consult [online documentation](http://cask.readthedocs.org/en/latest/guide/usage.html)

### Minimal Cask file

A new `Cask` file can be easily generated with the `init --dev` command.
```lisp
(source melpa)

(package-file "my-pkg.el")

(development
 (depends-on "ert")
```


See <http://cask.readthedocs.org/> for more information and
<https://groups.google.com/forum/#!forum/cask-dev> for development
related discussion.

## Install

To install `Cask`, run this command:

```bash
$ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
```

Or if you are on a Mac, you can install `Cask` via Homebrew:

```bash
$ brew install cask
```

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [COPYING](https://github.com/cask/cask/blob/master/COPYING) for details.
