Feature: Build
  Build package files

  Background:
    Given this Cask file:
      """
      (files "foo.el")
      """

  Scenario: No error or warning
    Given I create a file called "foo.el" with content:
      """
      (print "Hello World!")
      """
    When I run cask "build"
    Then last command exited with status code "0"

  Scenario: With warning
    Given I create a file called "foo.el" with content:
      """
      (next-line 1)
      """
    When I run cask "build"
    Then last command exited with status code "0"

  Scenario: With warning (with warn as error flag)
    Given I create a file called "foo.el" with content:
      """
      (next-line 1)
      """
    When I run cask "build --warn-as-error"
    Then last command exited with status code "1"

  Scenario: With error
    Given I create a file called "foo.el" with content:
      """
      (require 'does-not-exit)
      """
    When I run cask "build"
    Then last command exited with status code "1"
