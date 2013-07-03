Feature: Update
  In order to stay tuned
  As a Carton user
  I want to update packages

  Background:
    Given I create a project called "update"
    And I go to the project called "update"

  Scenario: No Carton file
    When I run carton "update"
    Then I should see command error:
      """
      Could not locate `Carton` file
      """

  @only-in-emacs-23
  Scenario: Update not supported
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run carton "install"
    Then there should exist a package directory called "foo-0.0.1"
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/new-packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run carton "update"
    And I should see command error:
      """
      The `update` command is not supported until Emacs 24.
      """

  @not-in-emacs-23
  Scenario: With dependency
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run carton "install"
    Then there should exist a package directory called "foo-0.0.1"
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/new-packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run carton "update"
    Then there should not exist a package directory called "foo-0.0.1"
    But there should exist a package directory called "foo-0.0.2"
    And I should see command output:
      """
      Updated packages:
      foo 0.0.1 -> 0.0.2
      """
