Feature: Outdated
  List outdated dependencies

  Scenario: Without outdated dependency
    Given this Caskfile:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "package-b" "0.0.1")
      """
    When I run cask "install"
    And I run cask "outdated"
    Then I should not see command output:
      """
      Outdated packages:
      """

  Scenario: With outdated dependency
    Given this Caskfile:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "package-a" "0.0.1")
      """
    When I run cask "install"
    Given this Caskfile:
      """
      (source "localhost" "http://127.0.0.1:9191/new-packages/")

      (depends-on "package-a" "0.0.1")
      """
    When I run cask "outdated"
    Then I should see command output:
      """
      Outdated packages:
      package-a 0.0.1 -> 0.0.2
      """
