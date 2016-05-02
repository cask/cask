Feature: Eval
  Link local packages

  Scenario: No arguments
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")
      """
    When I run cask "install"
    And I run cask "emacs -q --batch --eval '(print nil)'"
    Then I should not see command error:
      """
      Wrong type argument: stringp, nil
      """
    But I should see command output:
      """

      nil
      """
