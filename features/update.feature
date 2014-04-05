Feature: Update
  Update dependencies and list updated

  Scenario: With update
    Given this Caskfile:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "package-a" "0.0.1")
      """
    When I run cask "install"
    Then package "package-a-0.0.1" should be installed
    Given this Caskfile:
      """
      (source "localhost" "http://127.0.0.1:9191/new-packages/")

      (depends-on "package-a" "0.0.1")
      """
    When I run cask "update"
    Then package "package-a-0.0.2" should be installed
    And package "package-a-0.0.1" should not be installed
    And I should see command output:
      """
      Updated packages:
      package-a 0.0.1 -> 0.0.2
      """
