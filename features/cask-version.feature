Feature: Cask version

  Background:
    Given I create a project called "cask-version"
    And I go to the project called "cask-version"

  Scenario: Version
    When I run cask "--version"
    Then I should see cask version
