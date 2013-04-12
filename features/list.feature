Feature: List
  In order to quickly get an overview of all dependencies
  As a Carton user
  I want to list them

  Background:
    Given I create a project called "list"
    And I go to the project called "list"

  Scenario: No Carton file
    When I run carton "list"
    Then I should see command error:
      """
      Could not locate `Carton` file
      """

  Scenario: No dependencies
    Given this Carton file:
      """
      """
    When I run carton "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [0]:
      Development [0]:
      """

  Scenario: Single runtime dependency
    Given this Carton file:
      """
      (depends-on "foo" "0.1.2")
      """
    When I run carton "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [1]:
       - foo (0.1.2)

      Development [0]:
      """

  Scenario: Single development dependency
    Given this Carton file:
      """
      (development
       (depends-on "baz"))
      """
    When I run carton "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [0]:
      Development [1]:
       - baz
      """

  Scenario: Multiple runtime dependencies
    Given this Carton file:
      """
      (depends-on "foo" "0.1.2")
      (depends-on "bar" "0.2.1")
      """
    When I run carton "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [2]:
       - foo (0.1.2)
       - bar (0.2.1)

      Development [0]:
      """

  Scenario: Multiple development dependencies
    Given this Carton file:
      """
      (development
       (depends-on "baz")
       (depends-on "qux"))
      """
    When I run carton "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [0]:
      Development [2]:
       - baz
       - qux
      """

  Scenario: Multiple runtime and development dependencies
    Given this Carton file:
      """
      (depends-on "foo" "0.1.2")
      (depends-on "bar" "0.2.1")

      (development
       (depends-on "baz")
       (depends-on "qux"))
      """
    When I run carton "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [2]:
       - foo (0.1.2)
       - bar (0.2.1)

      Development [2]:
       - baz
       - qux
      """
