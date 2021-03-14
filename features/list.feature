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
      (source "localhost" "http://127.0.0.1:9191/packages/")
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

      Development [3]:
       - package-c
       - package-d (0.0.1)
       - package-f (0.0.1)
      """
