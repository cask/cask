Feature: List
  In order to quickly get an overview of the current project
  As a Cask user
  I want all the details

  Background:
    Given I create a project called "info"
    And I go to the project called "info"

  Scenario: No Cask file
    When I run cask "info"
    Then I should see no cask file error

  Scenario: Empty Cask file
    Given this Cask file:
      """
      """
    When I run cask "info"
    Then I should see command error:
      """
      Missing `package` or `package-file` directive
      """

  Scenario: Using package directive
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")
      """
    When I run cask "info"
    Then I should see command output:
      """
      ### super-project (0.0.1) ###

      Super project.
      """

  Scenario: Using package-file directive
    Given this Cask file:
      """
      (package-file "super-project.el")
      """
    When I create a file called "super-project.el" with content:
      """
      ;;; super-project.el --- Super project.

      ;; Copyright (C) 2013 Foo Barsson
      ;; Copyright (C) 2013 Baz Quxxon

      ;; Author: Foo Barsson <foo.bar@gmail.com>
      ;; Maintainer: Foo Barsson <foo.bar@gmail.com>
      ;; Version: 0.0.1
      ;; Keywords: examples
      ;; Package-Requires: ((baz "0.1.2") (qux "0.2.1"))
      ;; URL: http://github.com/foo/super-project

      ;; This file is NOT part of GNU Emacs.

      ;;; License:

      ;; This program is free software; you can redistribute it and/or modify
      ;; it under the terms of the GNU General Public License as published by
      ;; the Free Software Foundation; either version 3, or (at your option)
      ;; any later version.

      ;; This program is distributed in the hope that it will be useful,
      ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
      ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      ;; GNU General Public License for more details.

      ;; You should have received a copy of the GNU General Public License
      ;; along with GNU Emacs; see the file COPYING.  If not, write to the
      ;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
      ;; Boston, MA 02110-1301, USA.
      ;; Copyright (C) 2010-2013 Your Name

      ;; Author: Your Name <yourname@example.com>
      ;; Maintainer: Someone Else <someone@example.com>
      ;; Created: 14 Jul 2010
      ;; Keywords: foo

      ;;; Code:

      (provide 'super-project)

      ;;; super-project.el ends here
      """
    And I run cask "info"
    Then I should see command output:
      """
      ### super-project (0.0.1) ###

      Super project.
      """
