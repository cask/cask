Feature: Carton Deprecation
  Notify users about name switch

  Background:
    Given I create a project called "carton-deprecation"
    And I go to the project called "carton-deprecation"

  Scenario: Carton command
    When I run carton "info"
    Then I should see command error:
      """
      [DEPRECATION WARNING] The binary 'carton' is deprecated in favour of 'cask'
      """

  Scenario: Carton file
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then I should see command output:
      """
      [DEPRECATION WARNING] Rename the file 'Carton' to 'Cask'
      """
    And there should exist a package directory called "foo-0.0.1"
