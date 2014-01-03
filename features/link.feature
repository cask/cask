Feature: Link

  Background:
    Given I create a project called "link"
    And I go to the project called "link"

  Scenario: No Cask file
    When I run cask "link foo /path/to/foo"
    Then I should see no cask file error

  Scenario: Single link
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    When I run cask "install"
    And I run cask "link foo {{LINK-FOO}}"
    Then package "foo" should be linked to "{{LINK-FOO}}"

  Scenario: Multiple links
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      (depends-on "bar")
      """
    When I run cask "install"
    And I run cask "link foo {{LINK-FOO}}"
    And I run cask "link bar {{LINK-BAR}}"
    Then package "foo" should be linked to "{{LINK-FOO}}"
    And package "bar" should be linked to "{{LINK-BAR}}"

  Scenario: Path does not exist
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    When I run cask "install"
    And I run cask "link foo /path/to/non-existing-directory"
    Then I should see command error:
      """
      Cannot create link foo to non existing path: /path/to/non-existing-directory
      """

  Scenario: Package not a dependency
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    And I run cask "link bar {{LINK-BAR}}"
    Then I should see command error:
      """
      Cannot link package bar, is not a dependency
      """

  Scenario: Package already linked
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    When I run cask "install"
    And I run cask "link foo {{LINK-FOO}}"
    And I run cask "link foo {{LINK-NEW-FOO}}"
    Then package "foo" should be linked to "{{LINK-NEW-FOO}}"
