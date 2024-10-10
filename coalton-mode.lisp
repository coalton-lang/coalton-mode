;;;; Coalton LSP Server

(defpackage #:coalton-mode
  (:use #:cl)
  (:export #:main))

(in-package #:coalton-mode)

;;; Lifecycle protocol for process-like things: servers, sessions

(defgeneric start (process)
  (:documentation "Start a process. The started process is returned."))

(defgeneric stop (process)
  (:documentation "Stop a process. The stopped process is returned."))

;;; Messages

;;; Compile Coalton text, usually the full contents of a buffer.
;;;
;;; Return a structure:
;;;
;;;   (:result { "success" | "failure" }
;;;    :messages ((:message "message"
;;;                :type { "primary" | "secondary" }
;;;                :line line
;;;                :column column
;;;                :end-line end-line
;;;                :end-column end-column)
;;;               ... ))

#++
(defslimefun swank-coalton--compile (text)
  (let ((source (coalton-impl/source:make-source-string text)))
    (handler-case
        (progn
          (coalton-impl/entry:compile source :load t)
          (list :result "success"
                :messages nil))
      (coalton-impl/source:source-error (c)
        (list :result "failure"
              :messages (list (coalton-impl/source:export-condition c)))))))

#++ 
(defslimefun swank-coalton--codegen (text)
  (let ((source (coalton-impl/source:make-source-string text)))
    (coalton-impl/entry:codegen source)))


;;; Session

(defclass session ()
  ((input :initarg :input)
   (output :initarg :output)
   (socket :initarg :socket))
  (:documentation "Per-connection session data & runloop."))

;; start: when called, session has a new connection, from which
;; nothing has yet been read.

(defmethod start ((session session))
  ;; TODO implement me
  (stop session)
  )

(defmethod stop ((session session))
  (with-slots (socket) session
    (usocket:socket-close socket)))

;;; Server

(defclass server ()
  ((config :initarg :config)
   (listener :initform nil)
   (thread :initform nil)
   (lock :initform nil))
  (:documentation "A Coalton LSP server"))

(defmethod print-object ((self server) stream)
  (with-slots (config) self
    (destructuring-bind (&key host port &allow-other-keys) config
      (print-unreadable-object (self stream :type t :identity t)
        (format stream "~a:~a" host port)))))

;;; Server runloop: runs in a thread created during #'start

(defun %run (server)
  "Listening socket is open: wait for input, accept, repeat."
  (with-slots (listener lock) server
    (handler-case
        (loop :do
          (usocket:wait-for-input listener)
          (bt:with-recursive-lock-held (lock)
            (when (and listener (usocket::state listener))
              (let ((socket (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                (start (make-instance 'session
                         :input (flexi-streams:make-flexi-stream
                                 (usocket:socket-stream socket))
                         :output (usocket:socket-stream socket)
                         :socket socket))))))
      (usocket:bad-file-descriptor-error ()
        nil))))

(defmethod start ((server server))
  (with-slots (config listener thread lock) server
    (destructuring-bind (&key interface host port &allow-other-keys) config
      (setf listener (usocket:socket-listen (or interface host) port :reuse-address t)
            lock (bt:make-recursive-lock)
            thread (bt:make-thread (lambda () (%run server)) :name "Coalton Server"))))
  server)

(defmethod stop ((server server))
  (with-slots (config listener thread lock) server
    (destructuring-bind (&key host port &allow-other-keys) config
      (bt:with-recursive-lock-held (lock)
        (ignore-errors (usocket:socket-close (usocket:socket-connect host port)))
        (usocket:socket-close listener)
        (setf listener nil))))
  server)

;;; Main

(defvar *server* nil
  "The global server process.")

(defun main (&key port)
  "Run a Coalton server on PORT.  PORT must be specified."
  (cond (*server*
         (error "Server already running: ~a" *server*))
        ((null port)
         (error "Server TCP port must be specified"))
        (t
         (setf *server*
               (start (make-instance 'server
                        :config (list :port port
                                      :host "127.0.0.1"))))))
  (format t ";; Coalton Server started: ~a~%Shutdown with C-c.~%" *server*)
  (handler-case
      (loop (sleep 1))
    (sb-sys:interactive-interrupt ()
      (format t "~%;; Coalton Server halted.~%~%")
      (cl-user::quit))))

(defun halt ()
  "Kill the global server.  For exercising #'MAIN."
  (cond ((null *server*)
         (error "Server not running"))
        (t
         (stop *server*)
         (setf *server* nil))))
