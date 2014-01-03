Feature: Link List

  Background:
    Given I create a project called "link-list"
    And I go to the project called "link-list"

  Scenario: No Cask file
    When I run cask "link list"
    Then I should see no cask file error

  Scenario: No links
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    When I run cask "install"
    And I run cask "link list"
    Then I should not see any output

  Scenario: Single link
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo")
      """
    When I run cask "install"
    And I run cask "link foo {{LINK-FOO}}"
    And I run cask "link list"
    Then I should see links:
      | name | path         |
      | foo  | {{LINK-FOO}} |

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
    And I run cask "link list"
    Then I should see links:
      | name | path         |
      | foo  | {{LINK-FOO}} |
      | bar  | {{LINK-BAR}} |
