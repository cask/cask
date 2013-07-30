Feature: Init
  In order to quickly get going
  As a Cask user
  I want to create a Cask file automatically

  Background:
    Given I create a project called "init"
    And I go to the project called "init"

  Scenario: With Cask file
    Given this Cask file:
      """
      """
    When I run cask "init"
    Then I should see command error:
      """
      Cask file already exists.
      """

  Scenario: No Cask file - development
    When I run cask "init --dev"
    Then there should exist a file called "Cask" with this content:
      """
      (source melpa)

      (package "" "0.0.1" "")

      (depends-on "s" "1.3.0")
      (depends-on "dash" "1.0.3")

      (development
       (depends-on "ecukes")
       (depends-on "espuds"))
      """

  Scenario: No Cask file
    When I run cask "init"
    Then there should exist a file called "Cask" with this content:
      """
      (source melpa)

      (depends-on "cask")
      (depends-on "dash")
      (depends-on "drag-stuff")
      (depends-on "expand-region")
      (depends-on "magit")
      (depends-on "multiple-cursors")
      (depends-on "popwin")
      (depends-on "projectile")
      (depends-on "s")
      (depends-on "smex")
      (depends-on "wrap-region")
      (depends-on "yasnippet")
      """
