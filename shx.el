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

(defun shx--compile-cond-clause (outer-cond clause)
  "Compile a clause in a cond expression.
These are compiled as ELIF/ELSE clauses in an IF statement.
Should not be used for the first clause in the cond expression
for this reason.

OUTER-COND is the enclosing cond expression, used for error
reporting.  CLAUSE is a list of (test &rest body)."
  (cl-assert (listp clause) ()
             "Syntax error: cond clause is not a list\n\n  %s" clause)
  (cl-assert (car clause) ()
             "Syntax error: empty cond clause\n\n  %s" outer-cond)
  (cl-destructuring-bind (test &rest bod) clause
    (let ((b (->> (cons 'progn bod)
               shx--compile
               (s-chop-suffix ";"))))
      (if (-contains? '(t otherwise) test)
          (format "else %s;" b)
        (format "elif %s; then %s;" test b)))))

(defun shx--compile-cond (sexp)
  "Compile cond expression SEXP to an if-then-else form."
  (cl-assert (< 1 (length sexp)) ()
             "Syntax error: cond requires at least one clause\n\n  %s"
             sexp)
  (let ((clauses (cdr sexp)))
    (cl-destructuring-bind (c &rest cs) clauses
      (cl-assert (listp c) t
                 "Syntax error: cond clause is not a list\n\n %s")
      (cl-assert (car c) ()
                 "Syntax error: empty cond clause\n\n  %s" sexp)

      (cl-destructuring-bind (x &rest xs) c
        (let ((test (shx--compile x))
              (then (->> (cons 'progn xs)
                      shx--compile
                      (s-chop-suffix ";"))))
          (if (not cs)
              ;; One cond clause.
              (format "if %s; then %s; fi;" test then)
            ;; Several cond clauses.
            (let ((elifs
                   (->> cs
                     (--map (shx--compile-cond-clause sexp it))
                     (s-join " "))))

              (format "if %s; then %s; %s fi;" test then elifs))))))))

(defun shx--compile-list (sexp)
  "Compile SEXP as a list."
  (cl-case (car sexp)

    ((equal =)
     (cl-assert (equal 3 (length sexp)) ()
                "Syntax error: equal requires 2 arguments\n\n  %s"
                sexp)
     (format "[ %s -eq %s ]"
             (shx--compile (elt sexp 1))
             (shx--compile (elt sexp 2))))

    ((!= /=)
     (cl-assert (equal 3 (length sexp)) ()
                "Syntax error: /= requires 2 arguments\n\n  %s"
                sexp)
     (format "[ %s -ne %s ]"
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

    ((->)
     (cl-assert (< 2 (length sexp)) ()
                "Syntax error: -> requires 2 or more arguments\n\n  %s"
                sexp)
     (->> sexp
       (-drop 1)
       (-map 'shx--compile)
       (s-join " | ")))

    ((cond)
     (shx--compile-cond sexp))

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

;;;###autoload
(defmacro shx (form)
  "Convert FORM to a shell command and execute synchronously.
Return t or nil, depending on whether the command succeeded."
  (cl-assert form)
  (cl-assert (shx--compile form))
  `(let ((code (shell-command (shx--compile ',form))))
     (zerop code)))

;;;###autoload
(defmacro shx-string (form)
  "Convert FORM to a shell command and execute synchronously.
Return the result as a string."
  (cl-assert form)
  (cl-assert (shx--compile form))
  `(shell-command-to-string (shx--compile ',form)))

;;;###autoload
(defun shx-pp-to-string (sexp)
  "Compile SEXP and pretty-print as a string."
  (let ((s (->> (shx--compile sexp)
             (s-replace-all `((";" . ";\n")
                              ("if" . "\nif")
                              ("then " . "then\n  ")
                              ("else " . "else\n  "))))))
    (with-temp-buffer
      (insert s)
      (shell-script-mode)
      (goto-char (point-min))
      (while (not (eobp))
        (goto-char (line-beginning-position))
        (indent-for-tab-command)
        (forward-line))
      (buffer-string))))

;;;###autoload
(defun shx-pp (sexp)
  "Compile SEXP and pretty-print to a new buffer."
  (interactive "xExpression: ")
  (let ((s (shx-pp-to-string sexp))
        (buf (get-buffer-create "*shx pp output*")))
    (with-current-buffer buf
      (shell-script-mode)
      (erase-buffer)
      (insert s)
      (switch-to-buffer-other-window buf))))

(provide 'shx)

;;; shx.el ends here
