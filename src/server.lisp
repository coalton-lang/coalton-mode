;;;; Functions for starting and stoping the LSP server
;;;;
;;;; Use COALTON-LSP:MAIN to run from a shell, use START-SERVER to run
;;;; in a repl or slime process.

(in-package :coalton-lsp)

(defclass server (process)
  ((session-id :initform 0
               :accessor session-id)
   (config :initarg :config)
   (listener :initform nil)
   (sessions :initform nil))
  (:documentation "A Coalton LSP server"))

(defun server-address (server)
  "Return the string representation of SERVER's network address."
  (with-slots (config) server
    (destructuring-bind (&key host port &allow-other-keys) config
      (format nil "~a:~a" host port))))

(defmethod print-object ((self server) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (write-string (server-address self) stream)))

;;; Server run function: listen for and accept connections, and create
;;; sessions.

(defmethod run ((self server))
  (with-slots (listener sessions) self
    (handler-case
        (loop :do
          (usocket:wait-for-input listener)
          (with-lock-held (self)
            (when (and listener (usocket::state listener))
              (let ((socket (usocket:socket-accept listener
                                                   :element-type 'character)))
                (/debug "accept connection on ~a" socket)
                (create-session self socket)))))
      (usocket:bad-file-descriptor-error ()
        nil))))

(defun create-session (server socket)
  "Create a new session that communicates over SOCKET, and ad it to SERVER's session list."
  (push (start (make-instance 'session
                 :id (incf (session-id server))
                 :server server
                 :socket socket))
        (slot-value server 'sessions)))

(defun delete-session (server session)
  "Remove a previously stopped SESSION form server."
  (with-slots (lock sessions) server
    (bt:with-recursive-lock-held (lock)
      (delete session sessions))))

;;; Open the server port. The next-method will enter RUN.

(defmethod start ((server server))
  (with-slots (config listener thread) server
    (destructuring-bind (&key interface host port &allow-other-keys) config
      (setf listener (usocket:socket-listen (or interface host) port :reuse-address t))))
  (call-next-method))

(defmethod stop ((self server))
  (with-lock-held (self)
    (with-slots (sessions listener) self
      (when listener
        (usocket:socket-close listener)
        (setf listener nil))
      (dolist (session sessions)
        (stop session))))
  (call-next-method))

(defvar *default-port* 7887
  "The default port of LSP sessions.")

(defvar *server* nil
  "The server process.")

(defun stop-server ()
  "Close all sessions and stop the server."
  (when (null *server*)
    (/warn "server not running")
    (return-from stop-server))
  (stop *server*)
  (setf *server* nil)
  (/info "server halted"))

(defun start-server (&optional (port *default-port*))
  "Run a Coalton LSP server on PORT."
  (when *server*
    (/info "halting server at tcp:~a" (server-address *server*))
    (stop-server))
  (setf *server*
        (start (make-instance 'server
                 :config (list :port port
                               :host "127.0.0.1"))))
  (/info "server started at tcp:~a" (server-address *server*)))

(defun main (&key (port *default-port*))
  "Run a Coalton LSP server on PORT, halting on interrupt."
  (start-server port)
  (handler-case
      (loop (sleep 1))
    (sb-sys:interactive-interrupt ()
      (/info "server halted")
      (terpri)
      (cl-user::quit))))
