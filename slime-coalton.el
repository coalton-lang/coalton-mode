;;;; slime-coalton.el --- coalton-mode lisp integration -*- lexical-binding: t; -*-
;;;;
;;;; Slime extension via `define-slime-contrib' for interaction with a
;;;; Coalton instance running in a Slime-managed Lisp subprocess.

(require 'slime)

;; Ensure that slime is connected, coalton is loaded and swank components are loaded

(defun check-connection ()
  (unless (slime-connected-p)
    (error "Connect to slime."))
  ;; (eql :loaded (slime-eval `(swank:swank-coalton-status)))
  )

(cl-defmacro slime-coalton--show ((name) &body body)
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create ,name)
     (erase-buffer)
     (slime-popup-buffer-mode)
     ,@body
     (display-buffer (current-buffer))
     (current-buffer)))

(defun slime-coalton--buffer-name (type)
  (format "*coalton-%s*" (symbol-name type)))

(defun slime-coalton--popup-buffer (type)
  (let ((name (slime-coalton--buffer-name type)))
    (slime-coalton--show (name)
      (current-buffer))))

(defun slime-coalton--popup (type value)
  (pop-to-buffer (slime-coalton--popup-buffer type))
  (read-only-mode -1)
  (erase-buffer)
  (insert value)
  (goto-char (point-min)))

(defun slime-coalton--eval (sexp cont)
  (declare (indent 1))
  (check-connection)
  (slime-rex (cont)
      (sexp "swank")
    ((:ok result)
     (when cont
       (funcall cont result)))
    ((:abort condition)
     (message "Evaluation aborted on %s." condition))))

(defun coalton-ast ()
  "Display the AST of the current buffer."
  (interactive)
  (slime-coalton--eval `(swank:swank-coalton--codegen
                         ,(buffer-substring-no-properties (point-min) (point-max)))
    (lambda (result)
      (slime-coalton--popup 'ast result))))

(defun coalton-codegen ()
  "Display the compiled Lisp of the current buffer."
  (interactive)
  (slime-coalton--eval `(swank:swank-coalton--codegen
                         ,(buffer-substring-no-properties (point-min) (point-max)))
    (lambda (result)
      (slime-coalton--popup 'codegen result))))

(defun coalton-compile ()
  "Compile the current buffer."
  (interactive)
  (slime-coalton--eval `(swank:swank-coalton--compile
                         ,(buffer-substring-no-properties (point-min) (point-max)))
    (lambda (result)
      (slime-coalton--popup 'compile result))))

(defun coalton-compile-form ()
  "Redefine the toplevel form containing the current point."
  (interactive)
  (slime-coalton--eval `(swank:swank-coalton--compile-form
                         ,(buffer-substring-no-properties (point-min) (point-max))
                         (point))
    (lambda (result)
      (slime-coalton--popup 'compile result))))


;;; Initialization

(defun slime-coalton-init ()
  (message "slime-coalton.el: slime-coalton-init"))

(define-slime-contrib slime-coalton
  "Support Coalton language"
  (:authors "Jesse Bouwman <jlbouwman@hrl.com>")
  (:swank-dependencies swank::swank-coalton))

(provide 'slime-coalton)
