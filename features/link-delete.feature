Feature: Link Delete

  Background:
    Given I create a project called "link-delete"
    And I go to the project called "link-delete"

  Scenario: No Cask file
    When I run cask "link delete foo"
    Then I should see no cask file error

  Scenario: Existing link
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    When I run cask "install"
    And I run cask "link foo {{LINK-FOO}}"
    Then package "foo" should be linked to "{{LINK-FOO}}"
    When I run cask "link delete foo"
    Then package "foo" should not be linked

  Scenario: Non existing link
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    When I run cask "install"
    And I run cask "link delete foo"
    Then I should see command error:
      """
      Package foo not linked
      """

  Scenario: Package not a dependency
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    When I run cask "install"
    And I run cask "link delete bar"
    Then I should see command error:
      """
      Cannot link package bar, is not a dependency
      """
