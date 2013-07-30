Feature: Help
  In order to quickly get help
  As a Cask user
  I want to get usage infomartion

  Background:
    Given I create a project called "help"
    And I go to the project called "help"

  Scenario Outline: No Cask file
    When I run cask "<command>"
    Then I should see usage information

    Examples:
      | command |
      | help    |
      | -h      |
      | --help  |

  Scenario Outline: With Cask file
    Given this Cask file:
      """
      """
    When I run cask "<command>"
    Then I should see usage information

    Examples:
      | command |
      | help    |
      | -h      |
      | --help  |
