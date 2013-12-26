Feature: Load Path

  Background:
    Given I create a project called "load-path"
    And I go to the project called "load-path"

  Scenario: Print path
    When I run cask "load-path"
    Then I should see a colon path
