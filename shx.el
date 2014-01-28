;;; shx.el --- Write and run shell commands using s-expressions

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

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

;; Compiles s-expressions to shell commands, then executes them.

;;; Grammar:

;; ATOM     = SUBSHELL | STRING | SYMBOL | INT | t
;; LIST     = ( ATOM* )
;; SUBSHELL = [EXPR]
;; EXPR     = SUBSHELL | IF | WHEN | UNLESS | AND | OR | NOT | CASE
;;
;; PATH        = STRING
;; FILE_TEST   = (dir-exists? PATH)
;;             | (file-exists? PATH)
;;             | (file-executable? PATH)
;;             | (file-readable? PATH)
;;             | (file-writable? PATH)
;;
;; STRING_TEST = (s-blank? STRING)
;;             | (s-matches? REGEXP STRING)
;;             | (s-set? STRING)
;;             | (s-numeric? STRING)
;;
;; INT_TEST    = (= INT INT)
;;             | (/= INT INT)
;;             | (< INT INT)
;;             | (<= INT INT)
;;             | (> INT INT)
;;             | (>= INT INT)
;;             | (zero? INT)
;;             | (positive? INT)
;;             | (negative? INT)
;;
;; TEST = FILE_TEST | STRING_TEST | INT_TEST
;; NOT  = (not EXPR)
;; PRED = (not TEST) | TEST
;;
;; IF     = (if PRED EXPR EXPR)
;; WHEN   = (when PRED EXPR)
;; UNLESS = (unless PRED EXPR)
;;
;; COND_ELT       = (PRED EXPR)
;; COND_OTHERWISE = (t EXPR) | (otherwise EXPR)
;; COND           = (cond COND_ELT+ COND_OTHERWISE?)
;;
;; CASE_ELT       = (ATOM EXPR) | (LIST EXPR)
;; CASE_OTHERWISE = (t EXPR) | (otherwise EXPR)
;; CASE           = (case EXPR CASE_ELT+ CASE_OTHERWISE?)
;;
;; AND   = (and EXPR+)
;; OR    = (or EXPR+)
;; PROGN = (progn EXPR*)
;; PIPE  = (->> EXPR EXPR+)

;;; Code:

(eval-and-compile
  ;; Add cask packages to load path so flycheck checkers work.
  (when (boundp 'flycheck-emacs-lisp-load-path)
    (dolist (it (file-expand-wildcards "./.cask/*/elpa/*"))
      (add-to-list 'flycheck-emacs-lisp-load-path it))))

(require 'cl-lib)
(require 's)
(require 'dash)

(defun shx--compile-list (sexp)
  "Compile SEXP as a list."
  (cl-case (car sexp)

    ((equal)
     (cl-assert (equal 3 (length sexp)) ()
                "Syntax error: equal predicate requires 2 arguments\n\n  %s"
                sexp)
     (format "[ %s = %s ]"
             (shx--compile (elt sexp 1))
             (shx--compile (elt sexp 2))))

    ((not)
     (cl-assert (equal 2 (length sexp)) ()
                "Syntax error: not predicate requires 1 argument\n\n  %s"
                sexp)
     (format "[ ! %s ]" (shx--compile (elt sexp 1))))

    ((if)
     (cl-assert (equal 4 (length sexp)) ()
                "Syntax error: if statement requires 3 arguments:\n\n  %s"
                sexp)
     (format "if %s; then %s; else %s; fi"
             (shx--compile (elt sexp 1))
             (shx--compile (elt sexp 2))
             (shx--compile (elt sexp 3))))

    ((when)
     (cl-assert (< 2 (length sexp)) ()
                "Syntax error: when statement requires 2 or more arguments\n\n  %s"
                sexp)
     (format "if %s; then %s; fi"
             (shx--compile (elt sexp 1))
             (->> sexp
               (-drop 2)
               (-map 'shx--compile)
               (s-join "; "))))

    ((unless)
     (cl-assert (< 2 (length sexp)) ()
                "Syntax error: unless statement requires 2 or more arguments\n\n  %s"
                sexp)
     (format "if %s; then; else %s; fi"
             (shx--compile (elt sexp 1))
             (->> sexp
               (-drop 2)
               (-map 'shx--compile)
               (s-join "; "))))

    ((progn)
     (cl-assert (< 1 (length sexp)) ()
                "Syntax error: progn requires 1 or more arguments\n\n  %s"
                sexp)
     (->> sexp
       (-drop 1)
       (-map 'shx--compile)
       (s-join "; ")
       (s-append ";")))

    ((or)
     (cl-assert (< 2 (length sexp)) ()
                "Syntax error: or requires 2 or more arguments\n\n  %s"
                sexp)
     (->> sexp
       (-drop 1)
       (-map 'shx--compile)
       (s-join " || ")))

    ((and)
     (cl-assert (< 2 (length sexp)) ()
                "Syntax error: and requires 2 or more arguments\n\n  %s"
                sexp)
     (->> sexp
       (-drop 1)
       (-map 'shx--compile)
       (s-join " && ")))

    ((->>)
     (cl-assert (< 2 (length sexp)) ()
                "Syntax error: ->> requires 2 or more arguments\n\n  %s"
                sexp)
     (->> sexp
       (-drop 1)
       (-map 'shx--compile)
       (s-join " | ")))

    (t
     (error "Syntax error: Invalid expression\n\n  %s" sexp))))

(defun shx--compile (sexp)
  "Compile SEXP into a shell command string."
  (cond
   ((vectorp sexp)
    (let ((cmd (elt sexp 0)))
      (format "$(%s)" (shx--compile cmd))))
   ((listp sexp)
    (shx--compile-list sexp))
   ((integerp sexp)
    (number-to-string sexp))
   ((stringp sexp)
    sexp)
   (t
    (error "Syntax error: Invalid expression\n\n  %s" sexp))))

(defmacro shx (form)
  "Convert FORM to a shell command and execute synchronously.
Return t or nil, depending on whether the command succeeded."
  (cl-assert form)
  (cl-assert (shx--compile form))
  `(let ((code (shell-command (shx--compile ',form))))
     (zerop code)))

(defmacro shx-string (form)
  "Convert FORM to a shell command and execute synchronously.
Return the result as a string."
  (cl-assert form)
  (cl-assert (shx--compile form))
  `(shell-command-to-string (shx--compile ',form)))

(provide 'shx)

;;; shx.el ends here
