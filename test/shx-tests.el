;;; shx-tests.el --- Tests for shx.el

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for shx.el

;;; Code:

(require 'ert)

(ert-deftest compiles-square-bracketed-strings-to-subshells ()
  (should (equal "$(ls -la)" (shx--compile ["ls -la"]))))

;; if

(ert-deftest compiles-if-to-if-then-else ()
  (should (equal "if 0; then 1 else 2 fi"
                 (shx--compile '(if 0 1 2)))))

;; when

(ert-deftest compiles-when-to-if-then ()
  (should (equal "if 0; then 1 fi"
                 (shx--compile '(when 0 1)))))

(ert-deftest compiles-when-to-if-then-else--multiple-body-stmts ()
  (should (equal "if 0; then 1; 2; 3 fi"
                 (shx--compile '(when 0 1 2 3)))))

;; unless

(ert-deftest compiles-unless-to-if-then-else ()
  (should (equal "if 0; then; else 1 fi"
                 (shx--compile '(unless 0 1)))))

(ert-deftest compiles-unless-to-if-then-else--multiple-body-stmts ()
  (should (equal "if 0; then; else 1; 2; 3 fi"
                 (shx--compile '(unless 0 1 2 3)))))

;; predicates

(ert-deftest compiles-equality-predicate ()
  (should (equal "if [ -eq 0 1 ]; then 2 fi"
                 (shx--compile '(when (equal 0 1) 2)))))

;; progn

(ert-deftest compiles-progn-as-semicolon-delimited-statements ()
  (should (equal "0; 1; 2;"
                 (shx--compile '(progn 0 1 2)))))

;; ->>

(ert-deftest compiles-thread-as-pipe-delimited-statements ()
  (should (equal "0 | 1 | 2"
                 (shx--compile '(->> 0 1 2)))))

;; shx

(ert-deftest shx-executes-synchronously ()
  (should (equal (shell-command-to-string "uname")
                 (shx-string "uname"))))

(ert-deftest shx-executes-synchronously--t-on-success ()
  (should (shx "uname")))

(ert-deftest shx-executes-synchronously--nil-on-failure ()
  (should (not (eval `(shx ,(int-to-string (random)))))))

;; shx-string

(ert-deftest shx-executes-synchronously-returning-string ()
  (should (equal (shell-command-to-string "uname")
                 (shx-string "uname"))))

(provide 'shx-tests)

;;; shx-tests.el ends here
