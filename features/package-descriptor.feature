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
  When I create a file called "superpackage-pkg.el" with content:
  """
    (define-package "super-package" "1.2.3" "A Package that is really super."
     '((package-a "0.0.1")))
  """

  When I run cask "install"
  Then package "package-a-0.0.1" should be installed

Scenario: When no filename is specified and there is no -pkg.el file.
  Given this Cask file:
  """
  (package-descriptor)
  """

  When I run cask "install"
  Then I should see command error:
  """
  No -pkg.el file found for package descriptor
  """

Scenario: When a package descriptor file is explicitly specified
  Given this Cask file:
  """
  (source "localhost" "http://127.0.0.1:9191/packages/")
  (package-descriptor "unconventionally-named.el")
  """
  When I create a file called "unconventionally-named.el" with content:
  """
  (define-package "super-package" "1.2.3" "A Package that is really super."
    '((package-a "0.0.1")))
  """

  When I run cask "install"
  Then package "package-a-0.0.1" should be installed

Scenario: When a package descriptor file is explicitly specified but the file does not exist.
  Given this Cask file:
  """
  (package-descriptor "unconventionally-named.el")
  """

  When I run cask "install"
  Then I should see command error:
  """
  such file or directory
  """
