;;;; helpers for interactive development: starting network server,
;;;; changing logging options

(defpackage #:coalton-lsp-user
  (:use #:cl
        #:coalton-lsp))

(in-package #:coalton-lsp-user)

(defun restart-server ()
  (when *server*
    (stop-server))
  (start-server *server-port*))

;;; (restart-server)

(defun enable-debugging ()
  (set-log-level :debug))

(defun enable-worker-debugging ()
  (setf coalton-lsp::*worker-debug* t))

(defun enable-file-logger ()
  (set-log-file "~/git/coalton-mode/rpc.log"))
