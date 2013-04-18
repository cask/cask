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

  Scenario: With dependency
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run carton "install"
    Then there should exist a directory called "elpa/foo-0.0.1"
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/new-packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run carton "update"
    Then there should not exist a directory called "elpa/foo-0.0.1"
    But there should exist a directory called "elpa/foo-0.0.2"
