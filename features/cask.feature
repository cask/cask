Feature: Cask

  Scenario: Change default directory
    Given I create a project called "foo"
    And I go to the project called "foo"
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    Given I create a project called "bar"
    And I go to the project called "bar"
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "bar" "0.0.2")
      """
    When I run cask "install --path {{PROJECTS-PATH}}/foo"
    And I run cask "install --path {{PROJECTS-PATH}}/bar"
    When I go to the project called "foo"
    Then there should exist a package directory called "foo-0.0.1"
    When I go to the project called "bar"
    Then there should exist a package directory called "bar-0.0.2"
