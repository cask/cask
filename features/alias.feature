Feature: Source Alias
  In order to type less
  As a Carton user
  I want source alias

  Background:
    Given I create a project called "alias"
    And I go to the project called "alias"

  Scenario: Localhost
    Given this Carton file:
      """
      (source 'localhost)

      (depends-on "foo" "0.0.1")
      """
    When I run carton "install"
    Then there should exist a package directory called "foo-0.0.1"
