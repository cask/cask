Feature: Specify a package descriptor directly.

Most packages will use the dependencies listend in the main package file's
`Package-Requires` directive embedded in the magic comments
section. The dependencies listed in this fashion can only be
enumerated on a single line however, and so this can get quite out of
hand for packages that have a large list of dependencies.

In these cases, cask allows you to explicitly enumerate all of the
dependencies directly in elisp using the package descriptor and the
`define-package` function.

Scenario: When no filename is specified.
  Given this Cask file:
  """
  (source "localhost" "http://127.0.0.1:9191/packages/")
  (package-descriptor)
  """
  And a file named "superpackage-pkg.el"
    """
    (define-package "super-package" "1.2.3" "A Package that is really super."
     '((package-a "0.0.1")))
    """

  When I run cask "build"
  Then it builds a package named "superpackage"

  When I run cask "install"
  Then package "package-a" should be installed

Scenario: When a package descriptor file is explicitly specified
  Given this Cask file:
  """
  (source "localhost" "http://127.0.0.1:9191/packages/")
  (package-descriptor "unconventionally-named.el")
  """
  And this file named "unconventionally-named.el"
  """
  (define-package "super-package" "1.2.3" "A Package that is really super."
    '((package-a "0.0.1")))
  """

  When I run cask "build"
  Then it builds a package named "superpackage"

  When I run cask "install"
  Then package "package-a" should be installed
