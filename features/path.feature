Feature: Path

  Background:
    Given I create a project called "path"
    And I go to the project called "path"

  Scenario: Print Path
    When I run cask "path"
    Then I should see a colon path

  Scenario: Print Exec Path
    When I run cask "exec-path"
    Then I should see a colon path
