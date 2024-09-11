;;; swank-coalton.lisp

(in-package :swank)

(defun system-loaded-p (system-designator)
  (find system-designator (asdf:already-loaded-systems)
        :test #'string=))

(defun system-available-p (system-designator)
  (asdf:find-system system-designator))

(defun system-status (system-designator)
  (cond ((system-loaded-p system-designator)
         :loaded)
        ((system-available-p system-designator)
         :available)
        (t
         :unavailable)))


(defslimefun swank-coalton-status ()
  (system-status "coalton"))

(defslimefun swank-coalton-init ()
  (asdf:load-system "coalton"))


(defslimefun swank-coalton--codegen (text)
  (let ((source (coalton-impl/source:make-source-string text)))
    (coalton-impl/entry:codegen source)))

(defslimefun swank-coalton--compile (text)
  (let ((source (coalton-impl/source:make-source-string text)))
    (coalton-impl/entry:compile source :load t)
    "Success"))
