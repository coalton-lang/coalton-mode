;;;; Per-socket LSP session

(in-package :coalton-lsp)

(defclass session (process)
  ((id :initarg :id
       :reader session-id)
   (server :initarg :server)
   (socket :initarg :socket)
   (event-queue :initform (make-instance 'worker)
                :reader event-queue)
   (state :accessor session-state
          :initform 'uninitialized)
   (params :initform nil
           :accessor session-params)
   (documents :initform (make-hash-table :test #'equal)
              :accessor session-documents))
  (:documentation "Per-connection session data & runloop."))

(defmacro with-session-context ((session) &body body)
  `(with-logging-context (:session (lambda (stream)
                                     (format stream "session ~d" (session-id ,session))))
     ,@body))

(defun submit-event (session method value)
  (with-session-context (session)
    (/debug "submit-event ~a ~a" method value)
    (with-slots (event-queue) session
      (enqueue event-queue (cons method value)))))

(defun process-event (session event)
  (with-session-context (session)
    (destructuring-bind (method . value) event
      (/debug "process-event ~a" method)
      (funcall method session value))))

(defun session-uri (session)
  (cdr (assoc "root-uri" (session-params session) :test #'string=)))

(defun session-address (self)
  (with-slots (server id) self
    (format nil "~a:~a" (server-address server) id)))

(defmethod print-object ((self session) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a uri ~a" (session-address self) (session-uri self))))

(defun initializing-session (session params)
  (setf (session-state session) 'initializing)
  (setf (session-params session) params))

(defun initialized-session (session)
  (with-session-context (session)
    (setf (session-state session) 'initialized)
    (/info "initialized")
    nil))

(defun position-encoding (session)
  ':utf16)                              ; TODO consult capabilities

(defun update-configuration (session config)
  (with-session-context (session)
    (/info "updated configuration: ~a" config)
    nil))

;; key open documents by uri

(defun open-document (session document)
  (with-session-context (session)
    (let ((uri (cdr (assoc "uri" document :test #'string=))))
      (/info "open ~a" uri)
      (cond ((gethash uri (session-documents session))
             (/info "already open ~a" uri))
            (t
             (setf (gethash uri (session-documents session)) document)
             (submit-event session 'document-opened uri))))))

(defun change-document (session document) ; FIXME endpoint
  (with-session-context (session)
    (let ((uri (cdr (assoc "uri" document :test #'string=))))
      (submit-event session 'document-opened uri))))

(defclass uri-source ()
  ((uri :initarg :uri)))

(defmethod coalton-impl/source:source-stream ((self uri-source))
  (lib.uri:input-stream (slot-value self 'uri)))

(defmethod coalton-impl/source:source-available-p ((self uri-source))
  t)

(defmethod coalton-impl/source:source-name ((self uri-source))
  (lib.uri::uri-path (slot-value self 'uri)))

(defun export-condition (condition)
  "Extract text and position fields from a Coalton source condition."
  (with-open-stream (source-stream (coalton-impl/source::condition-stream condition))
    (let ((state (coalton-impl/source::make-printer-state source-stream condition)))
      (mapcar (lambda (note)
                (list (coalton-impl/source:message condition)
                      (coalton-impl/source:message note)
                      (coalton-impl/source::offset-position state
                                                            (coalton-impl/source::start-offset note))
                      (coalton-impl/source::offset-position state
                                                            (coalton-impl/source::end-offset note))))
              (coalton-impl/source::notes condition)))))

(defun make-diagnostic (s1 e1 s2 e2 message code)
  (let ((diagnostic (make-message 'diagnostic)))
    (set-field diagnostic (list :range :start :line) (1- s1))
    (set-field diagnostic (list :range :start :character) e1)
    (set-field diagnostic (list :range :end :line) (1- s2))
    (set-field diagnostic (list :range :end :character) e2)
    (set-field diagnostic (list :message) message)
    (set-field diagnostic (list :code) code)
    (set-field diagnostic (list :severity) :warning)
    (set-field diagnostic (list :source) "coalton")
    diagnostic))

(defun make-diagnostics (c)
  (mapcar (lambda (e)
            (let ((coalton-impl/settings:*coalton-print-unicode* nil)) ; -> ? wut
              (destructuring-bind (note message start end) e
                (message-value
                 (make-diagnostic (car start) (cdr start)
                                  (car end) (cdr end)
                                  (format nil "~a - ~a" note message)
                                  1)))))
          (export-condition c)))

(defun compile-uri (ur-uri)
  (let ((uri (lib.uri:parse ur-uri)))
    (when uri
      (let* ((filename (lib.uri::uri-path uri))
             (source (coalton-impl/source:make-source-file filename)))
        (handler-case
            (progn
              (coalton-impl/entry:compile source :load nil)
              nil)
          (coalton-impl/source::source-condition (c)
            (make-diagnostics c)))))))

(defun document-opened (session uri)
  (let* ((diagnostics (compile-uri uri))
         (message (make-message 'text-document-publish-diagnostics-params)))
    (set-field message :uri uri)
    (set-field message :diagnostics diagnostics)
    (let ((notification (make-notification "textDocument/publishDiagnostics" message)))
      (submit-event session 'write-message notification))))

(defun session-stream (session)
  (usocket:socket-stream (slot-value session 'socket)))

(define-condition session-exit ()
  ())

(defun make-request (rpc-message)
  (make-message (message-type rpc-message)
                (parsed-content rpc-message)))

;;; The message class of the 'result' field in a response message is
;;; that of the result message. Build a customized response-message by
;;; using that to set the field type so that the serializer can do its
;;; work. The only other thing needed is the associated request id.

(defun make-response (id result)
  (let ((class (copy-message (get-message-class 'response-message))))
    (set-field-class class :result (message-class result))
    (let ((response (make-instance 'message :class class)))
      (set-field response :jsonrpc "2.0")
      (set-field response :id id)
      (set-field response :result (message-value result))
      response)))

(defun make-error-response (id condition)
  (let ((response (make-message 'response-message)))
    (set-field response :jsonrpc "2.0")
    (set-field response :id id)
    (set-field response (list :error :code) (error-code condition))
    (set-field response (list :error :message) (error-message condition))
    response))

(defun make-notification (method params)
  (let ((class (copy-message (get-message-class 'notification-message))))
    (set-field-class class :params (message-class params))
    (let ((notification (make-instance 'message :class class)))
      (set-field notification :jsonrpc "2.0")
      (set-field notification :method method)
      (set-field notification :params (message-value params))
      notification)))

(defun message-p (message message-class)
  (eq (name (slot-value message 'class)) message-class))

(define-condition lsp-error (error)
  ((code :initarg :code
         :accessor error-code)
   (message :initarg :message
            :accessor error-message)))

(defun response-error (error-code args)
  (apply #'/error args)
  (error 'lsp-error
         :code error-code
         :message (apply #'format nil args)))

(defun invalid-request (&rest args)
  (response-error :invalid-request args))

(defun method-not-found (&rest args)
  (response-error :method-not-found args))

(defun request-method (request)
  (let ((rpc-version (get-field request :jsonrpc))
        (method (get-field request :method)))
    (cond ((not (string-equal rpc-version "2.0"))
           (invalid-request "Bad rpc version ~a" rpc-version))
          ((not method)
           (invalid-request "Missing method")))
    method))

(defvar *message-handlers*
  (make-hash-table :test 'equal))

(defstruct message-handler
  params
  fn)

(defun get-message-handler (method)
  (let ((handler (gethash method *message-handlers*)))
    (unless handler
      (method-not-found "Unsupported method '~a'" method))
    handler))

(defmacro define-handler (method params fn)
  `(setf (gethash ,method *message-handlers*)
         (make-message-handler :params ',params
                               :fn ',fn)))

(defun request-params (request)
  "Return REQUEST's params message."
  (let* ((method (request-method request))
         (params-message-class (message-handler-params (gethash method *message-handlers*))))
    (when params-message-class
      (make-message params-message-class (get-field request :params)))))

(defun process-notification (session request)
  (handler-case
      (let* ((handler (get-message-handler (request-method request)))
             (params (request-params request)))
        (funcall (message-handler-fn handler) session params))
    (lsp-error ()
      ;; logged when thrown
      )))

(defun process-request (session request)
  (handler-case
      (let* ((handler (get-message-handler (request-method request)))
             (params (request-params request))
             (result (funcall (message-handler-fn handler) session params)))
        (make-response (get-field request :id) result))
    (lsp-error (condition)
      (make-error-response (get-field request :id) condition))))

(defun process-message (session message)
  (let ((request (make-request message)))
    (cond ((message-p request 'notification-message)
           (process-notification session request))
          (t
           (submit-event session 'write-message
                         (process-request session request))))))

(defun write-message (session response)
  (let ((json (to-json response)))
    (with-lock-held (session)
      (write-rpc json (session-stream session)))))

(defmethod run ((self session))
  (with-slots (event-queue) self
    (setf (slot-value event-queue 'fn)
          (lambda (event)
            (process-event self event)))
    (start event-queue))
  (handler-case
      (loop :do
        (handler-case
            (progn
              (let ((message (read-rpc (session-stream self))))
                (submit-event self 'process-message message)))
          (sb-int:closed-stream-error ()
            (/info "remote session disconnected (stream closed)")
            (signal 'session-exit))
          (end-of-file ()
            (/info "remote session disconnected (end of file)")
            (signal 'session-exit))
          (error (c)
            (/error "aborted read: session shutdown: ~a" c)
            (signal 'session-exit))))
    (session-exit ()
      (stop self))))

(defmethod stop ((session session))
  (with-session-context (session)
    (/info "stopping")
    (with-slots (server socket event-queue) session
      (stop event-queue)
      (usocket:socket-close socket)
      (delete-session server session))
    (call-next-method)
    session))
