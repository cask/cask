Feature: Help
  In order to quickly get help
  As a Carton user
  I want to get usage infomartion

  Background:
    Given I create a project called "help"
    And I go to the project called "help"

  Scenario Outline: No Carton file
    When I run carton "<command>"
    Then I should see usage information

    Examples:
      | command |
      | help    |
      | -h      |
      | --help  |

  Scenario Outline: With Carton file
    Given this Carton file:
      """
      """
    When I run carton "<command>"
    Then I should see usage information

    Examples:
      | command |
      | help    |
      | -h      |
      | --help  |
