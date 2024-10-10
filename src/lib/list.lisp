(in-package #:coalton-lsp)

(defun listify (x)
  (if (listp x) x (list x)))
