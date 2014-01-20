Feature: List
  List package dependencies

  Scenario: No dependencies
    Given this Cask file:
      """
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [0]:
      Development [0]:
      """

  Scenario: With dependencies
    Given this Cask file:
      """
      (depends-on "foo" "0.1.2")
      (depends-on "bar" "0.2.1")

      (development
       (depends-on "baz")
       (depends-on "qux"))
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [2]:
       - bar (0.2.1)
       - foo (0.1.2)

      Development [2]:
       - qux
       - baz
      """
