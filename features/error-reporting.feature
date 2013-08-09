Feature: Error Reporting
  Report good errors about invalid Cask files

  Background:
    Given I create a project called "error-reporting"
    And I go to the project called "error-reporting"

  Scenario: Unbalanced Parenthesis
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (development
       (depends-on "f")
      """
    When I run cask "install"
    Then I should see command error:
       """
       Cask:3:1: unbalanced parenthesis
       """
    
