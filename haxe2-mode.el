;; Funda HaXe Mode
;; Version: 0.1.0
;; Author: Pierre Arlaud
;; URL: https://github.com/pierre-arlaud/funda-haxe-mode

;; ------------------------------------------------------------------------
;; Copyright (C) 2015 Pierre Arlaud

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope htat it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ------------------------------------------------------------------------

;; DESCRIPTION
;; Supports Syntax highlighting and indenting
;; Based on fundamental mode to avoid future breaks
;;
;; Syntax highlighting inspired by http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;; Indenting by http://stackoverflow.com/questions/4158216/emacs-custom-indentation
;; Syntax table inspired by http://emacswiki.org/emacs/ModeTutorial
;;

;; USAGE
;; (defvar haxe2-indent-offset 2) ; optional: change indent from 8 to 2
;; (require 'haxe2-mode)


;; Define regexps of the haxe grammar
(setq haxe2-namespace '("import" "package"))
(setq haxe2-classdef '("class" "interface" "enum" "typedef" "enum"))
(setq haxe2-scope-modifiers '("static" "public" "private" "override" "get" "set" "inline"))
(setq haxe2-accessors-scope '("get" "set" "default" "null" "never" "dynamic")) ;; `null` is redundant because it's already a constant
(setq haxe2-keywords '("for" "if" "switch" "while" "try" "catch" "do" "else" "case" "default"))
(setq haxe2-sub-keywords '("break" "continue" "return" "new" "in" "extends" "implements" "function" "var"))
(setq haxe2-constant-expressions '("false" "true" "null"))
(setq haxe2-primary-expressions '("this" "super"))

;; Regular expressions based on lists
(setq haxe2-namespace-regexp (regexp-opt haxe2-namespace 'words))
(setq haxe2-classdef-regexp (regexp-opt haxe2-classdef 'words))
(setq haxe2-scope-modifiers-regexp (regexp-opt haxe2-scope-modifiers 'words))
(setq haxe2-accessors-scope-regexp (regexp-opt haxe2-accessors-scope 'words))
(setq haxe2-keywords-regexp (regexp-opt haxe2-keywords 'words))
(setq haxe2-sub-keywords-regexp (regexp-opt haxe2-sub-keywords 'words))
(setq haxe2-constant-expressions-regexp (regexp-opt haxe2-constant-expressions 'words))
(setq haxe2-primary-expressions-regexp (regexp-opt haxe2-primary-expressions 'words))

;; Regular expressions a little more complicated

(setq haxe2-identifier-regexp "\\<\\([a-z][A-Za-z0-9_]*\\)\\>")
(setq haxe2-variable-regexp "\\<\\([A-Z_]*\\|[a-z][A-Za-z0-9_]*\\)\\>") ; constants support
(setq haxe2-classname-regexp "\\<\\([A-Z][A-Za-z0-9_]*\\)\\>")
(setq haxe2-param-regexp (concat "[,\\(][ \t]*" haxe2-identifier-regexp))

(setq haxe2-namespace-package-regexp (concat "import " haxe2-identifier-regexp))
(setq haxe2-vardef-regexp (concat "\\(var\\)[ \t]*" haxe2-variable-regexp))
(setq haxe2-functiondef-regexp (concat "\\(function\\)[ \t]*" haxe2-identifier-regexp))

;; Syntax Highlighting
(setq haxe2-font-lock-keywords
      `(

        (,haxe2-namespace-regexp (0 font-lock-keyword-face)
                                (,haxe2-identifier-regexp nil nil (0 font-lock-constant-face)))

        (,haxe2-vardef-regexp (1 font-lock-keyword-face) (2 font-lock-variable-name-face)
                             ;; Highlight possible accessors for the variable
                             (,haxe2-accessors-scope-regexp nil nil (0 font-lock-constant-face)))

        (,haxe2-functiondef-regexp (1 font-lock-keyword-face) (2 font-lock-function-name-face)
                                  ;; Highlight possible parameters as variable names
                                  (,haxe2-param-regexp nil ?( (1 font-lock-variable-name-face)))

        (,haxe2-classname-regexp . font-lock-type-face)
        (,haxe2-classdef-regexp . font-lock-keyword-face)
        (,haxe2-scope-modifiers-regexp . font-lock-keyword-face)
        (,haxe2-keywords-regexp . font-lock-keyword-face)
        (,haxe2-sub-keywords-regexp . font-lock-keyword-face)
        (,haxe2-primary-expressions-regexp . font-lock-keyword-face)
        (,haxe2-constant-expressions-regexp . font-lock-constant-face)
        ))


;; Syntax table
(defvar haxe2-mode-syntax-table
  (let ((haxe2-mode-syntax-table (make-syntax-table)))
    ;; Support C-style comments
    (modify-syntax-entry ?/ ". 124b" haxe2-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" haxe2-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" haxe2-mode-syntax-table)
	haxe2-mode-syntax-table)
"Syntax table for haxe2-mode")


;; Indenting
(defvar haxe2-indent-offset tab-width
  "*Indentation offset for `haxe2-mode'.")
(defun haxe2-indent-line ()
  "Indent current line for `haxe2-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[[{]")
              (setq indent-col (+ indent-col haxe2-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[]}]") (>= indent-col haxe2-indent-offset))
        (setq indent-col (- indent-col haxe2-indent-offset))))
    (indent-line-to indent-col)))


;; Mode definition
(define-derived-mode haxe2-mode fundamental-mode
  "haxe2 mode"
  "Fundamental Major mode for Haxe2"
  (kill-all-local-variables)
  (interactive)
  (setq font-lock-defaults '((haxe2-font-lock-keywords)))
  (set-syntax-table haxe2-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'haxe2-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (setq major-mode 'haxe2-mode
        mode-name "haxe2"
        local-abbrev-table haxe2-mode-abbrev-table
        abbrev-mode t)
)

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe2-mode))

(provide 'haxe2-mode)
