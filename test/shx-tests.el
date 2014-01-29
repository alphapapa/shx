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

(autoload 'shx-string "shx")
(autoload 'shx--compile "shx")
(autoload 'shx "shx")

;; sub

(ert-deftest compiles-sub-to-subshell ()
  (should (equal "$(ls -la)" (shx--compile '(sub "ls" "-la")))))

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
  (should (equal "[ 0 -eq 1 ]" (shx--compile '(equal 0 1))))
  (should (equal "[ 0 -eq 1 ]" (shx--compile '(= 0 1)))))

(ert-deftest error-if-not-2-args-to-equal ()
  (should-error (shx--compile '(equal)))
  (should-error (shx--compile '(equal 0)))
  (should-error (shx--compile '(equal 0 1 2)))

  (should-error (shx--compile '(=)))
  (should-error (shx--compile '(= 0)))
  (should-error (shx--compile '(= 0 1 2))))

;; /=

(ert-deftest compiles-/=-to-nequality-test ()
  (should (equal "[ 0 -ne 1 ]" (shx--compile '(/= 0 1))))
  (should (equal "[ 0 -ne 1 ]" (shx--compile '(!= 0 1)))))

(ert-deftest error-if-not-2-args-to-/= ()
  (should-error (shx--compile '(/=)))
  (should-error (shx--compile '(/= 0)))
  (should-error (shx--compile '(/= 0 1 2)))

  (should-error (shx--compile '(!=)))
  (should-error (shx--compile '(!= 0)))
  (should-error (shx--compile '(!= 0 1 2))))

;; <

(ert-deftest compiles-<-to-lt-test ()
  (should (equal "[ 0 -lt 1 ]"
                 (shx--compile '(< 0 1)))))

(ert-deftest error-if-not-2-args-to-< ()
  (should-error (shx--compile '(<)))
  (should-error (shx--compile '(< 0)))
  (should-error (shx--compile '(< 0 1 2))))

;; <=

(ert-deftest compiles-<=-to-le-test ()
  (should (equal "[ 0 -le 1 ]"
                 (shx--compile '(<= 0 1)))))

(ert-deftest error-if-not-2-args-to-<= ()
  (should-error (shx--compile '(<=)))
  (should-error (shx--compile '(<= 0)))
  (should-error (shx--compile '(<= 0 1 2))))

;; >

(ert-deftest compiles->-to-gt-test ()
  (should (equal "[ 0 -gt 1 ]"
                 (shx--compile '(> 0 1)))))

(ert-deftest error-if-not-2-args-to-> ()
  (should-error (shx--compile '(>)))
  (should-error (shx--compile '(> 0)))
  (should-error (shx--compile '(> 0 1 2))))

;; >=

(ert-deftest compiles->=-to-ge-test ()
  (should (equal "[ 0 -ge 1 ]"
                 (shx--compile '(>= 0 1)))))

(ert-deftest error-if-not-2-args-to->= ()
  (should-error (shx--compile '(>=)))
  (should-error (shx--compile '(>= 0)))
  (should-error (shx--compile '(>= 0 1 2))))

;; positive?

(ert-deftest compiles-positive?-to-gt-0-test ()
  (should (equal "[ 9 -gt 0 ]"
                 (shx--compile '(positive? 9)))))

(ert-deftest error-if-not-1-arg-to-positive? ()
  (should-error (shx--compile '(positive?)))
  (should-error (shx--compile '(positive? 0 1))))

;; zero?

(ert-deftest compiles-zero?-to-gt-0-test ()
  (should (equal "[ 9 -eq 0 ]"
                 (shx--compile '(zero? 9)))))

(ert-deftest error-if-not-1-arg-to-zero? ()
  (should-error (shx--compile '(zero?)))
  (should-error (shx--compile '(zero? 0 1))))

;; negative?

(ert-deftest compiles-negative?-to-lt-0-test ()
  (should (equal "[ 9 -lt 0 ]"
                 (shx--compile '(negative? 9)))))

(ert-deftest error-if-not-1-arg-to-negative? ()
  (should-error (shx--compile '(negative?)))
  (should-error (shx--compile '(negative? 0 1))))

;; dir-exists?

(ert-deftest compiles-dir-exists?-to-d-test ()
  (should (equal "[ -d 1 ]"
                 (shx--compile '(dir-exists? 1)))))

(ert-deftest error-if-not-1-arg-to-dir-exists? ()
  (should-error (shx--compile '(dir-exists?)))
  (should-error (shx--compile '(dir-exists? 0 1))))

;; f-exists?

(ert-deftest compiles-f-exists?-to-f-test ()
  (should (equal "[ -f 1 ]"
                 (shx--compile '(f-exists? 1)))))

(ert-deftest error-if-not-1-arg-to-f-exists? ()
  (should-error (shx--compile '(f-exists?)))
  (should-error (shx--compile '(f-exists? 0 1))))

;; f-nonempty?

(ert-deftest compiles-f-nonempty?-to-f-test ()
  (should (equal "[ -s 1 ]"
                 (shx--compile '(f-nonempty? 1)))))

(ert-deftest error-if-not-1-arg-to-f-nonempty? ()
  (should-error (shx--compile '(f-nonempty?)))
  (should-error (shx--compile '(f-nonempty? 0 1))))

;; f-readable?

(ert-deftest compiles-f-readable?-to-f-test ()
  (should (equal "[ -r 1 ]"
                 (shx--compile '(f-readable? 1)))))

(ert-deftest error-if-not-1-arg-to-f-readable? ()
  (should-error (shx--compile '(f-readable?)))
  (should-error (shx--compile '(f-readable? 0 1))))

;; f-writable?

(ert-deftest compiles-f-writable?-to-f-test ()
  (should (equal "[ -w 1 ]"
                 (shx--compile '(f-writable? 1)))))

(ert-deftest error-if-not-1-arg-to-f-writable? ()
  (should-error (shx--compile '(f-writable?)))
  (should-error (shx--compile '(f-writable? 0 1))))

;; f-executable?

(ert-deftest compiles-f-executable?-to-f-test ()
  (should (equal "[ -x 1 ]"
                 (shx--compile '(f-executable? 1)))))

(ert-deftest error-if-not-1-arg-to-f-executable? ()
  (should-error (shx--compile '(f-executable?)))
  (should-error (shx--compile '(f-executable? 0 1))))

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

;; infix expressions

(ert-deftest square-bracket-parsed-as-infix-exprs ()
  (should (equal "[ 1 -eq 2 ]" (shx--compile [1 = 2]))))

;; set!

(ert-deftest compiles-set!-to-var-assignment ()
  (should (equal "a=2" (shx--compile '(set! a 2)))))

(ert-deftest error-if-not-2-args-to-set! ()
  (should-error (shx--compile '(set!)))
  (should-error (shx--compile '(set! a)))
  (should-error (shx--compile '(set! a 1 2))))

(ert-deftest error-if-first-arg-to-set!-not-a-string-or-symbol ()
  (should (shx--compile '(set! a 1)))
  (should (shx--compile '(set! "a" 1)))
  (should-error (shx--compile '(set! 0 1))))

;; variable references

(ert-deftest plain-symbol-should-be-compiled-to-variable-reference ()
  (should (equal "${hello}" (shx--compile 'hello))))

;; calls

(ert-deftest list-with-sym-compiled-to-command-call ()
  (should (equal "echo hello world"
                 (shx--compile '(echo "hello" "world")))))

;; quotes

(ert-deftest quote-expanded-to-single-quoted-form ()
  (should (equal "'hello'" (shx--compile '(quote "hello"))))
  (should (equal "'hello'" (shx--compile '(% "hello")))))

(ert-deftest dquote-expanded-to-double-quoted-form ()
  (should (equal "\"hello\"" (shx--compile '(dquote "hello"))))
  (should (equal "\"hello\"" (shx--compile '(%% "hello")))))

;; export!

(ert-deftest compiles-unary-export!-to-exported-var ()
  (should (equal "export a" (shx--compile '(export! a)))))

(ert-deftest compiles-binary-export!-to-exported-var-assignment ()
  (should (equal "export a=2" (shx--compile '(export! a 2)))))

(ert-deftest error-if-not-1-or-2-args-to-export! ()
  (should-error (shx--compile '(export!)))
  (should-error (shx--compile '(export! a 1 2))))

(ert-deftest error-if-first-arg-to-export!-not-a-string-or-symbol ()
  (should (shx--compile '(export! a 1)))
  (should (shx--compile '(export! "a" 1)))
  (should-error (shx--compile '(export! 0 1))))

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
