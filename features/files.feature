Feature: Files

  Background:
    Given I create a project called "files"
    And I go to the project called "files"

  Scenario: No Cask file
    When I run cask "files"
    Then I should see no cask file error

  Scenario: No files directive
    Given this Cask file:
      """
      """
    When I create a file called "files.el"
    And I create a file called "files-core.el"
    And I create a file called "files-interactive.el"
    When I run cask "files"
    Then I should see command output:
      """
      files-core.el
      files-interactive.el
      files.el
      """

  Scenario: With files directive
    When I create a file called "files.el"
    And I create a file called "files-core.el"
    And I create a file called "files-interactive.el"
    And I create a directory called "bin"
    And I create a file called "bin/files"
    Given this Cask file:
      """
      (files "*.el" "bin")
      """
    When I run cask "files"
    Then I should see command output:
      """
      files-core.el
      files-interactive.el
      files.el
      bin
      """
