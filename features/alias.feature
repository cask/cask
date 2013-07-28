Feature: Source Alias
  In order to type less
  As a Cask user
  I want source alias

  Background:
    Given I create a project called "alias"
    And I go to the project called "alias"

  Scenario: Cask test
    Given this Cask file:
      """
      (source cask-test)

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then there should exist a package directory called "foo-0.0.1"

  Scenario: Invalid alias (quote)
    Given this Cask file:
      """
      (source 'cask-test)

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then I should see command error:
      """
      Unknown package archive: (quote cask-test)
      """

  Scenario: Invalid alias (string)
    Given this Cask file:
      """
      (source "cask-test")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then I should see command error:
      """
      Unknown package archive: cask-test
      """

  Scenario: Non existing alias
    Given this Cask file:
      """
      (source invalid)

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then I should see command error:
      """
      Unknown package archive: invalid
      """
