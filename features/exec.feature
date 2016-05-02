Feature: Exec
  Execute command with environment set up

  Scenario: No dependencies
    Given this Cask file:
      """
      """
    When I run cask "install"
    And I run cask "exec echo foo"
    Then I should see command output:
      """
      foo
      """

  Scenario: With dependency
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "package-c" "0.0.1")
      """
    When I run cask "install"
    When I run cask "exec {{EMACS}} --script .cask/{{EMACS-VERSION}}/elpa/package-c-0.0.1/package-c.el -Q --funcall hello"
    Then I should see command output:
      """
      Hello from PACKAGE-D, which is a PACKAGE-C dependency
      """

  Scenario: Binary in local package
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "package-e" "0.0.1")
      """
    When I run cask "install"
    When I run cask "exec hello"
    Then I should see command output:
      """
      Hello from PACKAGE-E
      """

  Scenario: Do not include bootstrap dependencies
    Given this Cask file:
      """
      """
    And I create a file called "foo.el" with content:
      """
      (require 's)
      """
    When I run cask "install"
    And I run cask "exec {{EMACS}} --script foo.el -Q"
    Then I should see command error:
      """
      Cannot open load file:
      """

  Scenario: Executable does not exist
    Given this Cask file:
      """
      """
    When I run cask "install"
    And I run cask "exec does-not-exist"
    Then I should see command error:
      """
      cask exec: error: Failed to execute does-not-exist: [Errno 2] No such file or directory
      Did you run cask install?
      """

  Scenario: install has not been run
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "package-c" "0.0.1")
      """
    When I run cask "exec true"
    Then I should see command error:
      """
      cask exec: error: Missing Packages
      package-c
      Do you need to run cask install?
      """
