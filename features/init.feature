Feature: Init
  In order to quickly get going
  As a Carton user
  I want to create a Carton file automatically

  Background:
    Given I create a project called "init"
    And I go to the project called "init"

  Scenario: With Carton file
    Given this Carton file:
      """
      """
    When I run carton "init"
    Then I should see command error:
      """
      Carton file already exists.
      """

  Scenario: No Carton file - development
    When I run carton "init --dev"
    Then there should exist a file called "Carton" with this content:
      """
      (source "melpa" "http://melpa.milkbox.net/packages/")

      (package "" "0.0.1" "")

      (depends-on "s" "1.3.0")
      (depends-on "dash" "1.0.3")

      (development
       (depends-on "ecukes")
       (depends-on "espuds"))
      """

  Scenario: No Carton file
    When I run carton "init"
    Then there should exist a file called "Carton" with this content:
      """
      (source "melpa" "http://melpa.milkbox.net/packages/")

      (depends-on "carton")
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
