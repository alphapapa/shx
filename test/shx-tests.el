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
  (should (equal "if 0; then 1; else 2; fi"
                 (shx--compile '(if 0 1 2)))))

(ert-deftest error-if-not-enough-args-to-if ()
  (should-error (shx--compile '(if)))
  (should-error (shx--compile '(if 0)))
  (should-error (shx--compile '(if 0 1))))

;; when

(ert-deftest compiles-when-to-if-then ()
  (should (equal "if 0; then 1; fi"
                 (shx--compile '(when 0 1)))))

(ert-deftest error-if-not-enough-args-to-when ()
  (should-error (shx--compile '(when)))
  (should-error (shx--compile '(when 0))))

(ert-deftest compiles-when-to-if-then-else--multiple-body-stmts ()
  (should (equal "if 0; then 1; 2; 3; fi"
                 (shx--compile '(when 0 1 2 3)))))

;; unless

(ert-deftest compiles-unless-to-if-then-else ()
  (should (equal "if 0; then; else 1; fi"
                 (shx--compile '(unless 0 1)))))

(ert-deftest error-if-not-enough-args-to-unless ()
  (should-error (shx--compile '(unless)))
  (should-error (shx--compile '(unless 0))))

(ert-deftest compiles-unless-to-if-then-else--multiple-body-stmts ()
  (should (equal "if 0; then; else 1; 2; 3; fi"
                 (shx--compile '(unless 0 1 2 3)))))

;; progn

(ert-deftest compiles-progn-as-semicolon-delimited-statements ()
  (should (equal "0; 1; 2;"
                 (shx--compile '(progn 0 1 2)))))

(ert-deftest error-if-not-enough-args-to-progn ()
  (should-error (shx--compile '(progn))))

;; ->

(ert-deftest compiles-thread-as-pipe-delimited-statements ()
  (should (equal "0 | 1 | 2"
                 (shx--compile '(-> 0 1 2)))))

(ert-deftest error-if-not-enough-args-to-thread ()
  (should-error (shx--compile '(->)))
  (should-error (shx--compile '(-> 0))))

;; or

(ert-deftest compiles-or-as-pipe-delimited-statements ()
  (should (equal "0 || 1 || 2"
                 (shx--compile '(or 0 1 2)))))

(ert-deftest error-if-not-enough-args-to-or ()
  (should-error (shx--compile '(or)))
  (should-error (shx--compile '(or 0))))

;; and

(ert-deftest compiles-and-as-pipe-delimited-statements ()
  (should (equal "0 && 1 && 2"
                 (shx--compile '(and 0 1 2)))))

(ert-deftest error-if-not-enough-args-to-and ()
  (should-error (shx--compile '(and)))
  (should-error (shx--compile '(and 0))))

;; equal

(ert-deftest compiles-equal-to-equality-test ()
  (should (equal "[ 0 -eq 1 ]"
                 (shx--compile '(equal 0 1)))))

(ert-deftest error-if-not-2-args-to-equal ()
  (should-error (shx--compile '(equal)))
  (should-error (shx--compile '(equal 0)))
  (should-error (shx--compile '(equal 0 1 2))))

;; =

(ert-deftest compiles-=-to-equality-test ()
  (should (equal "[ 0 -eq 1 ]"
                 (shx--compile '(= 0 1)))))

(ert-deftest error-if-not-2-args-to-= ()
  (should-error (shx--compile '(=)))
  (should-error (shx--compile '(= 0)))
  (should-error (shx--compile '(= 0 1 2))))

;; /=

(ert-deftest compiles-/=-to-nequality-test ()
  (should (equal "[ 0 -ne 1 ]"
                 (shx--compile '(/= 0 1)))))

(ert-deftest error-if-not-2-args-to-/= ()
  (should-error (shx--compile '(/=)))
  (should-error (shx--compile '(/= 0)))
  (should-error (shx--compile '(/= 0 1 2))))

;; !=

(ert-deftest compiles-!=-to-nequality-test ()
  (should (equal "[ 0 -ne 1 ]"
                 (shx--compile '(!= 0 1)))))

(ert-deftest error-if-not-2-args-to-!= ()
  (should-error (shx--compile '(!=)))
  (should-error (shx--compile '(!= 0)))
  (should-error (shx--compile '(!= 0 1 2))))

;; not

(ert-deftest compiles-not-predicate ()
  (should (equal "[ ! 0 ]"
                 (shx--compile '(not 0)))))

(ert-deftest error-if-not-1-arg-to-not ()
  (should-error (shx--compile '(not)))
  (should-error (shx--compile '(not 0 1))))

;; cond

(ert-deftest compiles-cond--single-clause--to-if ()
  (should (equal "if 0; then 1; fi;"
                 (shx--compile '(cond (0 1))))))

(ert-deftest compiles-cond--single-clause--multiple-statements--to-if ()
  (should (equal "if 0; then 1; 2; fi;"
                 (shx--compile '(cond (0 1 2))))))

(ert-deftest compiles-cond--with-t-clause--to-if-then-else ()
  (should (equal "if 0; then 1; else 2; fi;"
                 (shx--compile '(cond (0 1) (t 2))))))

(ert-deftest compiles-cond--with-otherwise-clause--to-if-then-else ()
  (should (equal "if 0; then 1; else 2; fi;"
                 (shx--compile '(cond (0 1) (otherwise 2))))))

(ert-deftest error-if-not-1-arg-to-cond ()
  (should-error (shx--compile '(cond))))

(ert-deftest error-if-cond-clause-not-list ()
  (should-error (shx--compile '(cond 0))))

(ert-deftest error-if-cond-clause-is-empty ()
  (should-error (shx--compile '(cond ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
