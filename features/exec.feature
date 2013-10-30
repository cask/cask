Feature: Exec
  In order to run with correct dependencies
  As a Cask user
  I want to setup the correct load path

  Background:
    Given I create a project called "exec"
    And I go to the project called "exec"

  Scenario: No Cask file
    When I run cask "exec echo foo"
    Then I should see command output:
      """
      foo
      """

  Scenario: No dependencies
    Given this Cask file:
      """
      """
    When I run cask "exec echo foo"
    Then I should see command output:
      """
      foo
      """

  Scenario: With dependency
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "baz" "0.0.3")
      """
    When I run cask "install"
    When I run cask "exec {{EMACS}} --script .cask/{{EMACS-VERSION}}/elpa/baz-0.0.3/baz.el -Q --funcall hello"
    Then I should see command output:
      """
      Hello from QUX, which is a BAZ dependency
      """

  Scenario: Binary in local package
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "hey" "0.0.5")
      """
    When I run cask "install"
    When I run cask "exec hey"
    Then I should see command output:
      """
      Hello from HEY
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
    When I run cask "exec does-not-exist"
    Then I should see command error:
      """
      cask exec: error: [Errno 2] No such file or directory
      """
