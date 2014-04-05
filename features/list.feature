Feature: List
  List package dependencies

  Scenario: No dependencies
    Given this Caskfile:
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
    Given this Caskfile:
      """
      (depends-on "package-a" "0.1.2")
      (depends-on "package-b" "0.2.1")

      (development
       (depends-on "package-c")
       (depends-on "package-d"))
      """
    When I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [2]:
       - package-b (0.2.1)
       - package-a (0.1.2)

      Development [2]:
       - package-d
       - package-c
      """
