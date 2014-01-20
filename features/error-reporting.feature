Feature: Error Reporting
  Report good errors about invalid Cask files

  Scenario: Unbalanced Parenthesis
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (development
       (depends-on "f")
      """
    When I run cask "install"
    Then I should see command error:
       """
       /Cask:3:1: End of file while reading (possible unbalanced parenthesis)
       """

  Scenario: Invalid read syntax
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (development
       (depends-on "f"'))
      """
    When I run cask "install"
    Then I should see command error:
      """
      /Cask:4:19: Invalid read syntax: ")"
      """

  Scenario: Installation failure (dep dependency does not exist)
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "dep")
      """
    When I run cask "install"
    Then I should see command error:
      """
      Dependency dep failed to install: Package `invalid-0.0.1' is unavailable
      """
