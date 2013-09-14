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
      (s-upcase "this should fail")
      """
    When I run cask "install"
    And I run cask "exec {{EMACS}} --script foo.el -Q"
    Then I should see command error:
      """
      Symbol's function definition is void: s-upcase
      """
