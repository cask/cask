Feature: Version
  Show package version

  Scenario: Show package version
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")
      """
    When I run cask "version"
    Then I should see command output:
      """
      0.0.1
      """
