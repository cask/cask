Feature: Package directory
  As a Carton user
  I want to be able to print the package directory

  Background:
    Given I create a project called "package-directory"
    And I go to the project called "package-directory"

  Scenario: Package directory
    Given this Carton file:
      """
      """
    When I run carton "package-directory"
    Then I should see command output:
      """
      {{PROJECT-PATH}}/.carton/{{EMACS-VERSION}}/elpa
      """
