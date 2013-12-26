Feature: Update
  In order to stay tuned
  As a Cask user
  I want to update packages

  Background:
    Given I create a project called "update"
    And I go to the project called "update"

  Scenario: No Cask file
    When I run cask "update"
    Then I should see no cask file error

  Scenario: With dependency
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then there should exist a package directory called "foo-0.0.1"
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/new-packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "update"
    Then there should not exist a package directory called "foo-0.0.1"
    But there should exist a package directory called "foo-0.0.2"
    And I should see command output:
      """
      Updated packages:
      foo 0.0.1 -> 0.0.2
      """
