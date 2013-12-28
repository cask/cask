Feature: Link

  Background:
    Given I create a project called "link"
    And I go to the project called "link"

  Scenario: No Cask file - no name
    When I run cask "link"
    Then I should see no cask file error

  Scenario: No Cask file - with name
    When I run cask "link foo"
    Then I should see no cask file error
