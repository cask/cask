Feature: Outdated
  In order to stay tuned
  As a Cask user
  I want to outdated packages

  Background:
    Given I create a project called "outdated"
    And I go to the project called "outdated"

  Scenario: No Cask file
    When I run cask "outdated"
    Then I should see command error:
      """
      Could not locate `Cask` file
      """

  Scenario: Without outdated dependency
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "bar" "0.0.2")
      """
    When I run cask "install"
    Then there should exist a package directory called "bar-0.0.2"
    When I run cask "outdated"
    Then I should not see command output:
      """
      Outdated packages:
      """

  Scenario: With outdated dependency
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
    When I run cask "outdated"
    Then I should see command output:
      """
      Outdated packages:
      foo 0.0.1 -> 0.0.2
      """
