Feature: Help
  Show usage information

  Scenario Outline: No Cask file
    When I run cask "<command>"
    Then I should see usage information

    Examples:
      | command |
      | help    |
      | -h      |
      | --help  |

  Scenario Outline: With Cask file
    Given this Caskfile:
      """
      """
    When I run cask "<command>"
    Then I should see usage information

    Examples:
      | command |
      | help    |
      | -h      |
      | --help  |
