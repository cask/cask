Feature: Update
  Update dependencies and list updated

  Scenario: With update
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then package "foo-0.0.1" should be installed
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/new-packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "update"
    Then package "foo-0.0.2" should be installed
    And package "foo-0.0.1" should not be installed
    And I should see command output:
      """
      Updated packages:
      foo 0.0.1 -> 0.0.2
      """
