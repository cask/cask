Feature: Exec
  In order to run with correct dependencies
  As a Carton user
  I want to setup the correct load path

  Background:
    Given I create a project called "exec"
    And I go to the project called "exec"

  Scenario: No Carton file
    When I run carton "exec echo foo"
    Then I should see command output:
      """
      foo
      """

  Scenario: No dependencies
    Given this Carton file:
      """
      """
    When I run carton "exec echo foo"
    Then I should see command output:
      """
      foo
      """

  Scenario: With dependency
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "baz" "0.0.3")
      """
    When I run carton "install"
    When I run carton "exec {{EMACS}} --script .carton/{{EMACS-VERSION}}/elpa/baz-0.0.3/baz.el -Q --funcall hello"
    Then I should see command output:
      """
      Hello from QUX, which is a BAZ dependency
      """
