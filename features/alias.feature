Feature: Source Alias
  In order to type less
  As a Carton user
  I want source alias

  Background:
    Given I create a project called "alias"
    And I go to the project called "alias"

  Scenario: Carton test
    Given this Carton file:
      """
      (source carton-test)

      (depends-on "foo" "0.0.1")
      """
    When I run carton "install"
    Then there should exist a package directory called "foo-0.0.1"

  Scenario: Invalid alias (quote)
    Given this Carton file:
      """
      (source 'carton-test)

      (depends-on "foo" "0.0.1")
      """
    When I run carton "install"
    Then I should see command error:
      """
      Unknown package archive: (quote carton-test)
      """

  Scenario: Invalid alias (string)
    Given this Carton file:
      """
      (source "carton-test")

      (depends-on "foo" "0.0.1")
      """
    When I run carton "install"
    Then I should see command error:
      """
      Unknown package archive: carton-test
      """

  Scenario: Non existing alias
    Given this Carton file:
      """
      (source invalid)

      (depends-on "foo" "0.0.1")
      """
    When I run carton "install"
    Then I should see command error:
      """
      Unknown package archive: invalid
      """
