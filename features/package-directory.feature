Feature: Package directory
  As a Cask user
  I want to be able to print the package directory

  Background:
    Given I create a project called "package-directory"
    And I go to the project called "package-directory"

  Scenario: Package directory
    Given this Cask file:
      """
      """
    When I run cask "package-directory"
    Then I should see command output:
      """
      {{PROJECT-PATH}}/.cask/{{EMACS-VERSION}}/elpa
      """
