Feature: Missing
  List missing dependencies

  Scenario: No dependencies
    Given this Cask file:
    """
    (source "localhost" "http://127.0.0.1:9191/packages/")
    """
    When I run cask "missing"
    Then I should see no command output

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
    When I run cask "install"
    And I run cask "missing"
    Then I should see no command output

  Scenario: With missing dependencies
    Given this Cask file:
    """
    (source "localhost" "http://127.0.0.1:9191/packages/")
    (depends-on "package-a" "0.1.2")
    (depends-on "package-b" "0.2.1")

    (development
      (depends-on "package-c")
      (depends-on "package-d"))
    """
    When I run cask "missing"
    Then I should see command output:
    """
    package-b
    package-a
    package-d
    package-c
    """
