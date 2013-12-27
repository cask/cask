Feature: Clean elc

  Background:
    Given I create a project called "clean-elc"
    And I go to the project called "clean-elc"

  Scenario: No Cask file
    When I run cask "clean-elc"
    Then I should see no cask file error

  Scenario: No files directive
    Given this Cask file:
      """
      """
    When I run cask "clean-elc"
    Then I should not see any output

  Scenario: With files directive
    When I create a file called "files.el"
    And I create a file called "files-core.el"
    And I create a directory called "src"
    And I create a file called "src/misc.el"
    And I create a file called "src/foo.el"
    Given this Cask file:
      """
      (files "files*.el" "src/misc.el")
      """
    When I run cask "build"
    Then there should exist a file called "files.elc"
    And there should exist a file called "files-core.elc"
    And there should exist a file called "src/misc.elc"
    But there should not exist a file called "src/foo.elc"
    When I run cask "clean-elc"
    Then there should not exist a file called "files.elc"
    And there should not exist a file called "files-core.elc"
    And there should not exist a file called "src/misc.elc"
    And there should not exist a file called "src/foo.elc"
