;;;; coalton-mode.el --- Major mode for working with Coalton -*- lexical-binding: t; -*-
;;;; Commentary:
;;;;
;;;; URL: http://github.com/coalton-lang/coaltom
;;;; Keywords: languages Coalton Lisp
;;;; Version: 1.0.0
;;;; Package-Requires: ((Emacs "26.3"))
;;;;
;;;; This file contains functions for in-Emacs structural operations on
;;;; Coalton code, including syntax highlighting, indentation and
;;;; navigation, and command integration with the in-CL operations
;;;; defined in `slime-coalton.el'.

(require 'lisp-mnt)
(require 'eglot)

;;; Code:

(add-to-list 'eglot-server-programs
             ;; '(coalton-mode . ("coalton-lsp"))
             '(coalton-mode . ("localhost" 7887)))

(defconst coalton-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name))))

(defvar coalton-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") 'coalton-ast)
    (define-key map (kbd "C-c C-g") 'coalton-codegen)
    (define-key map (kbd "C-c C-l") 'coalton-compile)
    (define-key map (kbd "C-c C-c") 'coalton-compile-form)
    map))

(defvar coalton-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry '(0 . 127) "_" table)

    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)

    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    table))

(defvar coalton--debug nil
  "Enable debugging.")


;; Initialization

(defun coalton-mode-variables ()
  "Initialize buffer-local vars."
  (setq-local comment-start "; "))

;;;###autoload
(define-derived-mode coalton-mode prog-mode "Coalton"
  "Major mode for working with Coalton.

\\{coalton-mode-map}"
  :syntax-table coalton-mode-syntax-table)

(add-to-list 'auto-mode-alist '("\\.coal\\'" . coalton-mode))

(provide 'coalton-mode)

;;; coalton-mode.el ends here
