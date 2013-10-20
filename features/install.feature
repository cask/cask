Feature: Install
  In order to satisfy dependencies
  As a Cask user
  I want to install them

  Background:
    Given I create a project called "install"
    And I go to the project called "install"

  Scenario: No Cask file
    When I run cask "install"
    Then I should see command error:
      """
      Could not locate `Cask` file
      """

  Scenario: No dependencies
    Given this Cask file:
      """
      """
    When I run cask "install"
    Then package directory should not exist

  Scenario: Single dependency
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask "install"
    Then there should exist a package directory called "foo-0.0.1"

  Scenario: Multiple dependencies
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      (depends-on "bar" "0.0.2")
      """
    When I run cask "install"
    Then there should exist a package directory called "foo-0.0.1"
    And there should exist a package directory called "bar-0.0.2"

  Scenario: Missing dependencies
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "missing-a" "0.0.1")
      (depends-on "missing-b" "0.0.2")
      """
    When I run cask "install"
    Then there should not exist a package directory called "missing-a-0.0.1"
    And there should not exist a package directory called "missing-b-0.0.2"
    And I should see command error:
      """
      Some dependencies were not available: missing-a, missing-b
      """

  Scenario: Using package-file directive
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

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
      ;; Package-Requires: ((foo "0.0.1") (bar "0.0.2"))
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
    And I run cask "install"
    Then there should exist a package directory called "foo-0.0.1"
    And there should exist a package directory called "bar-0.0.2"

  Scenario: No argument to binary is same as install
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (depends-on "foo" "0.0.1")
      """
    When I run cask ""
    Then there should exist a package directory called "foo-0.0.1"
