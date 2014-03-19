Feature: Info

  Scenario: Show project information
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")
      """
    When I run cask "info"
    Then I should see command output:
      """
      ### super-project (0.0.1) ###

      Super project.
      """
