Feature: List
  In order to quickly get an overview of all dependencies
  As a Cask user
  I want to list them

  Background:
    Given I create a project called "list"
    And I go to the project called "list"

  Scenario: No Cask file
    When I run cask "list"
    Then I should see no cask file error

  Scenario: No dependencies
    Given this Cask file:
      """
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [0]:
      Development [0]:
      """

  Scenario: Single runtime dependency
    Given this Cask file:
      """
      (depends-on "foo" "0.1.2")
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [1]:
       - foo (0.1.2)

      Development [0]:
      """

  Scenario: Single development dependency
    Given this Cask file:
      """
      (development
       (depends-on "baz"))
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [0]:
      Development [1]:
       - baz
      """

  Scenario: Multiple runtime dependencies
    Given this Cask file:
      """
      (depends-on "foo" "0.1.2")
      (depends-on "bar" "0.2.1")
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [2]:
       - bar (0.2.1)
       - foo (0.1.2)

      Development [0]:
      """

  Scenario: Multiple development dependencies
    Given this Cask file:
      """
      (development
       (depends-on "baz")
       (depends-on "qux"))
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [0]:
      Development [2]:
       - qux
       - baz
      """

  Scenario: Multiple runtime and development dependencies
    Given this Cask file:
      """
      (depends-on "foo" "0.1.2")
      (depends-on "bar" "0.2.1")

      (development
       (depends-on "baz")
       (depends-on "qux"))
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [2]:
       - bar (0.2.1)
       - foo (0.1.2)

      Development [2]:
       - qux
       - baz
      """
