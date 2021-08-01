Feature: Error Reporting
  Report good errors about invalid Cask files

  Scenario: Unbalanced Parenthesis
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (development
       (depends-on "f")
      """
    When I run cask "install"
    Then I should see command error:
       """
       /Cask:3:1: End of file while reading (possible unbalanced parenthesis)
       """

  Scenario: Invalid read syntax
    Given this Cask file:
      """
      (source "localhost" "http://127.0.0.1:9191/packages/")

      (development
       (depends-on "f"'))
      """
    When I run cask "install"
    Then I should see command error:
      """
      /Cask:4:19: Invalid read syntax: ")"
      """

  Scenario: Invalidly balanced package requires
    Given I create a file called "foo.el" with content:
      """
      ;;; foo.el --- Foo

      ;; Copyright (C) 2015 Peter Pan

      ;; Author: Peter Pan <peter@pan.com>
      ;; Maintainer: Peter Pan <peter@pan.com>
      ;; Version: 0.0.1
      ;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (f "0.14.0") (emacs "24")

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

      ;;; Commentary:

      ;;; Code:

      (provide 'foo)

      ;;; foo.el ends here
      """
    And this Cask file:
      """
      (package-file "foo.el")
      """
    When I run cask "install"
    Then I should see command error:
      """
      Unbalanced parens in Package-Requires
      """
