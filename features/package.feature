Feature: Package

  Background:
    Given I create a project called "package"
    And I go to the project called "package"

  Scenario: No Cask file
    When I run cask "package"
    Then I should see no cask file error

  Scenario: Empty Cask file
    Given this Cask file:
      """
      """
    When I run cask "package"
    Then I should see command error:
      """
      Missing `package` or `package-file` directive
      """

  Scenario: Single file
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")
      """
    And I create a file called "super-project.el"
    When I run cask "package"
    Then there should exist a file called "dist/super-project-0.0.1.el"

  Scenario: Multiple files
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")
      """
    And I create a file called "super-project.el"
    And I create a file called "super-project-core.el"
    When I run cask "package"
    Then there should exist a file called "dist/super-project-0.0.1.tar"

  Scenario: Target directory
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")
      """
    And I create a file called "super-project.el"
    When I run cask "package foo"
    Then there should exist a file called "foo/super-project-0.0.1.el"
