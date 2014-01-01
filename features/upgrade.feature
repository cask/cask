Feature: Upgrade

  Scenario: Deprecation warning
    When I run cask "upgrade"
    Then I should see command error:
      """
      The upgrade command is deprecated in favor of upgrade-cask
      """
