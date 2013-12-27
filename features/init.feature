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
      Cask-file already exists
      """

  Scenario: No Cask file - development
    When I run cask "init --dev"
    Then there should exist a file called "Cask" with this content:
      """
      (source gnu)
      (source melpa)

      (package-file "TODO")

      (development
       (depends-on "f")
       (depends-on "ecukes")
       (depends-on "ert-runner")
       (depends-on "el-mock"))
      """

  Scenario: No Cask file
    When I run cask "init"
    Then there should exist a file called "Cask" with this content:
      """
      (source gnu)
      (source melpa)

      (depends-on "bind-key")
      (depends-on "cask")
      (depends-on "dash")
      (depends-on "drag-stuff")
      (depends-on "exec-path-from-shell")
      (depends-on "expand-region")
      (depends-on "f")
      (depends-on "flycheck")
      (depends-on "flycheck-cask")
      (depends-on "htmlize")
      (depends-on "idle-highlight-mode")
      (depends-on "magit")
      (depends-on "multiple-cursors")
      (depends-on "nyan-mode")
      (depends-on "pallet")
      (depends-on "popwin")
      (depends-on "prodigy")
      (depends-on "projectile")
      (depends-on "s")
      (depends-on "smartparens")
      (depends-on "smex")
      (depends-on "use-package")
      (depends-on "web-mode")
      (depends-on "yasnippet")
      """
