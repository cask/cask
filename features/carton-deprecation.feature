Feature: Carton Deprecation
  Notify users about name switch

  Background:
    Given I create a project called "carton-deprecation"
    And I go to the project called "carton-deprecation"

  Scenario: Carton command
    When I run carton "info"
    Then I should see command error:
      """
      [DEPRECATION WARNING] The binary 'carton' is deprecated in favour of 'cask'
      """

  Scenario: Carton file
    Given this Carton file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then I should see command output:
      """
      [DEPRECATION WARNING] Rename the file 'Carton' to 'Cask'
      """
    And there should exist a package directory called "foo-0.0.1"

  Scenario: Carton elpa path
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    And I move ".cask/{{EMACS-VERSION}}/elpa" to "elpa"
    And I run cask "load-path"
    Then I should see command output:
      """
      {{PROJECT-PATH}}/elpa
      """
    And I should see command output:
      """
      [DEPRECATION WARNING] Remove 'elpa' directory and run 'cask' command again
      """

  Scenario: Favor .cask over elpa directory
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    And I move ".cask/{{EMACS-VERSION}}/elpa" to "elpa"
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      (depends-on "bar" "0.0.2")
      """
    When I run cask "install"
    And I run cask "list"
    Then I should see command output:
      """
      ### Dependencies ###

      Runtime [2]:
       - foo (0.0.1)
       - bar (0.0.2)

      Development [0]:
      """
