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
    When I run carton "exec echo foo"
    Given this Carton file:
      """
      """
    When I run carton "exec echo foo"
    Then I should see command output:
      """
      foo
      """

  Scenario: Global binary
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "baz" "0.0.3")
      """
    When I run carton "install"
    When I run carton "exec emacs --script elpa/baz-0.0.3/baz.el -Q"
    Then I should see command output:
      """
      Hello from QUX, which is a BAZ dependency
      """

  Scenario: Local elpa package binary
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "hey" "0.0.5")
      """
    When I run carton "install"
    When I run carton "exec hey"
    Then I should see command output:
      """
      Hello from HEY
      """

  Scenario: Path to local elpa package binary
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "hey" "0.0.5")
      """
    When I run carton "install"
    When I run carton "exec elpa/hey-0.0.5/bin/hey"
    Then I should see command output:
      """
      Hello from HEY
      """
