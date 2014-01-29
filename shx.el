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

;; Compiles s-expressions to shell commands. The command below will compile the
;; `progn' expression to a shell script and pretty-print it to a new buffer.
;;
;;   (shx-pp
;;    '(progn
;;       (set! EXAMPLE 20)
;;       (cond ([EXAMPLE = 20]
;;              (echo "success"))
;;             ((f-exists? "~/Desktop/hello")
;;              (set! STR (sub "ls -la"))
;;              (echo "works"))
;;             (t
;;              (echo EXAMPLE)))))
;;

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
  (let ((test (car clause))
        (body (->> (cons 'progn (cdr clause))
                shx--compile
                (s-chop-suffix ";"))))
    (if (-contains? '(t otherwise) test)
        (format "else %s;" body)
      (format "elif %s; then %s;" (shx--compile test) body))))

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
  (cl-destructuring-bind (cmd &rest args) sexp
    (cl-case cmd

      ;; Equality

      ((equal =)
       (cl-assert (equal 2 (length args)) ()
                  "Syntax error: equal requires 2 arguments\n\n  %s"
                  sexp)
       (format "[ %s -eq %s ]"
               (shx--compile (elt args 0))
               (shx--compile (elt args 1))))

      ((!= /=)
       (cl-assert (equal 2 (length args)) ()
                  "Syntax error: /= requires 2 arguments\n\n  %s"
                  sexp)
       (format "[ %s -ne %s ]"
               (shx--compile (elt args 0))
               (shx--compile (elt args 1))))

      ;; Relations

      ((<)
       (cl-assert (equal 2 (length args)) ()
                  "Syntax error: < requires 2 arguments\n\n  %s"
                  sexp)
       (format "[ %s -lt %s ]"
               (shx--compile (elt args 0))
               (shx--compile (elt args 1))))

      ((<=)
       (cl-assert (equal 2 (length args)) ()
                  "Syntax error: <= requires 2 arguments\n\n  %s"
                  sexp)
       (format "[ %s -le %s ]"
               (shx--compile (elt args 0))
               (shx--compile (elt args 1))))

      ((>)
       (cl-assert (equal 2 (length args)) ()
                  "Syntax error: > requires 2 arguments\n\n  %s"
                  sexp)
       (format "[ %s -gt %s ]"
               (shx--compile (elt args 0))
               (shx--compile (elt args 1))))

      ((>=)
       (cl-assert (equal 2 (length args)) ()
                  "Syntax error: >= requires 2 arguments\n\n  %s"
                  sexp)
       (format "[ %s -ge %s ]"
               (shx--compile (elt args 0))
               (shx--compile (elt args 1))))

      ((positive?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: positive? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ %s -gt 0 ]" (shx--compile (car args))))

      ((zero?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: zero? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ %s -eq 0 ]" (shx--compile (car args))))

      ((negative?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: negative? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ %s -lt 0 ]" (shx--compile (car args))))

      ;; IO tests

      ((dir-exists?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: dir-exists? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ -d %s ]" (shx--compile (car args))))

      ((f-exists?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: f-exists? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ -f %s ]" (shx--compile (car args))))

      ((f-nonempty?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: f-nonempty? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ -s %s ]" (shx--compile (car args))))

      ((f-writable?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: f-writable? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ -w %s ]" (shx--compile (car args))))

      ((f-readable?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: f-readable? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ -r %s ]" (shx--compile (car args))))

      ((f-executable?)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: f-executable? requires 1 argument\n\n  %s"
                  sexp)
       (format "[ -x %s ]" (shx--compile (car args))))

      ;; Logic

      ((or)
       (cl-assert (<= 2 (length args)) ()
                  "Syntax error: or requires 2 or more arguments\n\n  %s"
                  sexp)
       (s-join " || " (-map 'shx--compile args)))

      ((and)
       (cl-assert (<= 2 (length args)) ()
                  "Syntax error: and requires 2 or more arguments\n\n  %s"
                  sexp)

       (s-join " && " (-map 'shx--compile args)))

      ((not)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: not predicate requires 1 argument\n\n  %s"
                  sexp)
       (format "[ ! %s ]" (shx--compile (car args))))

      ;; Control structures

      ((if)
       (cl-assert (equal 3 (length args)) ()
                  "Syntax error: if statement requires 3 arguments:\n\n  %s"
                  sexp)
       (format "if %s; then %s; else %s; fi"
               (shx--compile (elt args 0))
               (shx--compile (elt args 1))
               (shx--compile (elt args 2))))

      ((when)
       (cl-assert (<= 2 (length args)) ()
                  "Syntax error: when statement requires 2 or more arguments\n\n  %s"
                  sexp)
       (format "if %s; then %s; fi"
               (shx--compile (car args))
               (s-join "; " (-map 'shx--compile (cdr args)))))

      ((unless)
       (cl-assert (<= 2 (length args)) ()
                  "Syntax error: unless statement requires 2 or more arguments\n\n  %s"
                  sexp)
       (format "if %s; then; else %s; fi"
               (shx--compile (car args))
               (s-join "; " (-map 'shx--compile (cdr args)))))

      ((progn)
       (cl-assert (<= 1 (length args)) ()
                  "Syntax error: progn requires 1 or more arguments\n\n  %s"
                  sexp)
       (->> args (-map 'shx--compile) (s-join "; ") (s-append ";")))

      ;; Quoting

      ((quote %)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: quote requires 1 argument\n\n  %s" sexp)
       (format "'%s'" (shx--compile (car args))))

      ((dquote %%)
       (cl-assert (equal 1 (length args)) ()
                  "Syntax error: quote requires 1 argument\n\n  %s" sexp)
       (format "\"%s\"" (shx--compile (car args))))

      ;; Shell features

      ((set!)
       (cl-assert (equal 2 (length args)) ()
                  "Syntax error: set! requires 2 arguments\n\n  %s" sexp)
       (let ((var (car args)))
         (cl-assert (or (symbolp var) (stringp var)) ()
                    "Syntax error: first argument to set! must be a string or symbol\n\n  %s"
                    sexp)
         (format "%s=%s"
                 (car args)
                 (shx--compile (elt args 1)))))

      ((export!)
       (cl-assert (-contains? '(1 2) (length args)) ()
                  "Syntax error: export! requires 1 or 2 arguments\n\n  %s" sexp)
       (let ((var (car args)))
         (cl-assert (or (symbolp var) (stringp var)) ()
                    "Syntax error: first argument to export! must be a string or symbol\n\n  %s"
                    sexp)
         (if (equal 2 (length args))
             (format "export %s=%s" var (shx--compile (elt args 1)))
           (format "export %s" var))))

      ((sub)
       (cl-assert (<= 1 (length args)) ()
                  "Syntax error: sub requires an argument\n\n  %s"
                  sexp)
       (format "$(%s)" (s-join " " args)))

      ((->)
       (cl-assert (<= 2 (length args)) ()
                  "Syntax error: -> requires 2 or more arguments\n\n  %s"
                  sexp)
       (s-join " | " (-map 'shx--compile args)))

      ((cond)
       (shx--compile-cond sexp))

      (t
       (cond ((symbolp cmd)
              (format "%s %s"
                      cmd
                      (->> args (-map 'shx--compile) (s-join " "))))
             (t
              (error "Syntax error: Invalid expression\n\n  %s" sexp)))))))

(defun shx--compile (sexp)
  "Compile SEXP into a shell command string."
  (cond
   ((vectorp sexp)
    (cl-destructuring-bind (fst op &rest rest) (mapcar 'identity sexp)
      (shx--compile (cl-list* op fst rest))))
   ((listp sexp)
    (shx--compile-list sexp))
   ((integerp sexp)
    (number-to-string sexp))
   ((stringp sexp)
    sexp)
   ((symbolp sexp)
    (format "${%s}" sexp))
   (t
    (error "Syntax error: Invalid expression\n\n  %s" sexp))))

;;;###autoload
(defmacro shx (form)
  "Convert FORM to a shell command and execute synchronously.
Return t or nil, depending on whether the command succeeded."
  (cl-assert form)
  (let ((s (shx--compile form)))
    (cl-assert s)
    `(zerop (shell-command ,s))))

;;;###autoload
(defmacro shx-string (form)
  "Convert FORM to a shell command and execute synchronously.
Return the result as a string."
  (cl-assert form)
  (let ((s (shx--compile form)))
    (cl-assert s)
    `(shell-command-to-string ,s)))

;;;###autoload
(defun shx-pp-to-string (sexp)
  "Compile SEXP and pretty-print as a string."
  (let ((s (->> (shx--compile sexp)
             (s-replace-all `((";" . ";\n")
                              ("then " . "then\n  ")
                              ("else " . "else\n  "))))))
    (with-temp-buffer
      (insert s)
      (shell-script-mode)
      (goto-char (point-min))

      ;; Remove unneeded semicolons.
      (save-excursion
        (while (search-forward-regexp (rx (not (any "]")) (group ";") eol)
                                      nil t)
          (replace-match "" nil nil nil 1)))

      ;; Join 'then' onto same line as 'if'.
      (save-excursion
        (while (search-forward-regexp (rx bol (* space) "then" (* space) eol)
                                      nil t)
          (join-line)))

      ;; Indent buffer.
      (save-excursion
        (while (not (eobp))
          (goto-char (line-beginning-position))
          (indent-for-tab-command)
          (forward-line)))

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
