Feature: Load path option
  Extend load path with directory

  Background:
    Given this Cask file:
      """
      (files "lisp/foo.el")
      """
    And I create a directory called "lisp"
    And I create a file called "lisp/foo.el" with content:
      """
      (require 'bar)
      """
    And I create a file called "lisp/bar.el" with content:
      """
      (provide 'bar)
      """

  Scenario: Not in load-path
    When I run cask "build"
    Then I should see command output:
      """
      Cannot open load file: no such file or directory, bar
      """

  Scenario: Added to load-path
    When I run cask "build --load-path lisp"
    Then I should see command output pattern:
      """
      Compiling .+foo.el
      """
    And I should see command output pattern:
      """
      Wrote .+foo.elc
      """
