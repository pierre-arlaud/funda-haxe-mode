;; HaXe Mode by Pierre Arlaud
;; Supports Syntax highlighting and indenting
;; Based on fundamental mode to avoid future breaks
;;
;; Syntax highlighting inspired by http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;; Indenting by http://stackoverflow.com/questions/4158216/emacs-custom-indentation
;; Syntax table inspired by http://emacswiki.org/emacs/ModeTutorial
;;

;; Define regexps of the haxe grammar
(setq haxe-namespace '("import" "package"))
(setq haxe-classdef '("class" "interface" "enum" "typedef" "enum"))
(setq haxe-scope-modifiers '("static" "public" "private" "override" "get" "set" "inline"))
(setq haxe-accessors-scope '("get" "set" "default" "null" "never" "dynamic")) ;; `null` is redundant because it's already a constant
(setq haxe-keywords '("for" "if" "switch" "while" "try" "catch" "do" "else" "case" "default"))
(setq haxe-sub-keywords '("break" "continue" "return" "new" "in" "extends" "implements" "function" "var"))
(setq haxe-constant-expressions '("false" "true" "null"))
(setq haxe-primary-expressions '("this" "super"))

;; Regular expressions based on lists
(setq haxe-namespace-regexp (regexp-opt haxe-namespace 'words))
(setq haxe-classdef-regexp (regexp-opt haxe-classdef 'words))
(setq haxe-scope-modifiers-regexp (regexp-opt haxe-scope-modifiers 'words))
(setq haxe-accessors-scope-regexp (regexp-opt haxe-accessors-scope 'words))
(setq haxe-keywords-regexp (regexp-opt haxe-keywords 'words))
(setq haxe-sub-keywords-regexp (regexp-opt haxe-sub-keywords 'words))
(setq haxe-constant-expressions-regexp (regexp-opt haxe-constant-expressions 'words))
(setq haxe-primary-expressions-regexp (regexp-opt haxe-primary-expressions 'words))

;; Regular expressions a little more complicated

(setq haxe-identifier-regexp "\\<\\([a-z][A-Za-z0-9_]*\\)\\>")
(setq haxe-variable-regexp "\\<\\([A-Z_]*\\|[a-z][A-Za-z0-9_]*\\)\\>") ; constants support
(setq haxe-classname-regexp "\\<\\([A-Z][A-Za-z0-9_]*\\)\\>")
(setq haxe-param-regexp (concat "[,\\(][ \t]*" haxe-identifier-regexp))

(setq haxe-namespace-package-regexp (concat "import " haxe-identifier-regexp))
(setq haxe-vardef-regexp (concat "\\(var\\)[ \t]*" haxe-variable-regexp))
(setq haxe-functiondef-regexp (concat "\\(function\\)[ \t]*" haxe-identifier-regexp))

;; Syntax Highlighting
(setq haxe-font-lock-keywords
      `(

        (,haxe-namespace-regexp (0 font-lock-keyword-face)
                                (,haxe-identifier-regexp nil nil (0 font-lock-constant-face)))

        (,haxe-vardef-regexp (1 font-lock-keyword-face) (2 font-lock-variable-name-face)
                             ;; Highlight possible accessors for the variable
                             (,haxe-accessors-scope-regexp nil nil (0 font-lock-constant-face)))
        
        (,haxe-functiondef-regexp (1 font-lock-keyword-face) (2 font-lock-function-name-face)
                                  ;; Highlight possible parameters as variable names
                                  (,haxe-param-regexp nil ?( (1 font-lock-variable-name-face)))
                                  
        (,haxe-classname-regexp . font-lock-type-face)
        (,haxe-classdef-regexp . font-lock-keyword-face)
        (,haxe-scope-modifiers-regexp . font-lock-keyword-face)
        (,haxe-keywords-regexp . font-lock-keyword-face)
        (,haxe-sub-keywords-regexp . font-lock-keyword-face)
        (,haxe-primary-expressions-regexp . font-lock-keyword-face)
        (,haxe-constant-expressions-regexp . font-lock-constant-face)
        ))


;; Syntax table
(defvar haxe-mode-syntax-table
  (let ((haxe-mode-syntax-table (make-syntax-table)))
    ;; Support C-style comments
    (modify-syntax-entry ?/ ". 124b" haxe-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" haxe-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" haxe-mode-syntax-table)
	haxe-mode-syntax-table)
"Syntax table for haxe-mode")


;; Indenting
(defvar haxe-indent-offset tab-width
  "*Indentation offset for `haxe-mode'.")
(defun haxe-indent-line ()
  "Indent current line for `haxe-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[[{]")
              (setq indent-col (+ indent-col haxe-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[]}]") (>= indent-col haxe-indent-offset))
        (setq indent-col (- indent-col haxe-indent-offset))))
    (indent-line-to indent-col)))


;; Mode definition
(define-derived-mode haxe-mode fundamental-mode
  "haxe mode"
  "Fundamental Major mode for Haxe"
  (kill-all-local-variables)
  (interactive)
  (setq font-lock-defaults '((haxe-font-lock-keywords)))
  (set-syntax-table haxe-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'haxe-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (setq major-mode 'haxe-mode
        mode-name "haXe"
        local-abbrev-table haxe-mode-abbrev-table
        abbrev-mode t)
)

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))

(provide 'funda-haxe-mode)
