Feature: Pkg file
  In order to quickly create a -pkg.el file
  As a Cask user
  I want to create it automatically

  Background:
    Given I create a project called "pkg-file"
    And I go to the project called "pkg-file"

  Scenario: No Cask file
    When I run cask "pkg-file"
    Then I should see no cask file error

  Scenario: Empty Cask file
    Given this Cask file:
      """
      """
    When I run cask "pkg-file"
    Then I should see command error:
      """
      Missing `package` or `package-file` directive
      """

  Scenario: Using package directive no dependencies
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")
      """
    When I run cask "pkg-file"
    Then there should exist a file called "super-project-pkg.el" with this content:
      """
      (define-package "super-project" "0.0.1" "Super project." 'nil)
      """

  Scenario: Using package directive with dependencies
    Given this Cask file:
      """
      (package "super-project" "0.0.1" "Super project.")

      (depends-on "foo" "0.1.2")
      (depends-on "bar" "0.2.1")
      """
    When I run cask "pkg-file"
    Then there should exist a file called "super-project-pkg.el" with this content:
      """
      (define-package "super-project" "0.0.1" "Super project."
        '((bar "0.2.1")
          (foo "0.1.2")))
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
    And I run cask "pkg-file"
    Then there should exist a file called "super-project-pkg.el" with this content:
      """
      (define-package "super-project" "0.0.1" "Super project."
        '((qux "0.2.1")
          (baz "0.1.2")))
      """
