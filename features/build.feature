Feature: Build

  Background:
    Given I create a project called "build"
    And I go to the project called "build"

  Scenario: No Cask file
    When I run cask "build"
    Then I should see no cask file error

  Scenario: No files
    Given this Cask file:
      """
      """
    When I run cask "build"
    Then I should not see any output

  Scenario: With files directive
    When I create a file called "files.el"
    And I create a file called "files-core.el"
    And I create a file called "files.txt"
    And I create a directory called "src"
    And I create a file called "src/misc.el"
    Given this Cask file:
      """
      (files "*.txt" "*.el" "src/*.el")
      """
    When I run cask "build"
    Then there should exist a file called "files.elc"
    And there should exist a file called "files-core.elc"
    And there should exist a file called "src/misc.elc"
