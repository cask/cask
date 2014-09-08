Feature: Link
  Link local packages

  Scenario: No arguments
    When I run cask "link"
    Then I should not see command error:
      """
      Wrong type argument: stringp, nil
      """
    But I should see command output:
      """
      Manage links.
      """
