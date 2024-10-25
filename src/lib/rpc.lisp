;;;; Read and write JSON-RPC messages
;;;;
;;;; https://www.jsonrpc.org/specification

(in-package :coalton-lsp)

(defun read-header (stream)
  "Read a HTTP-header-formatted key value pair from STREAM."
  (declare (optimize (speed 3)))
  (flet ((make-buf ()
           (make-array 0 :adjustable t :fill-pointer t :element-type 'character)))
    (let ((state :begin-line)
          (prev-state nil)
          (buf (make-buf))
          (k nil))
      (loop :for c := (read-char stream)
            :do (ecase state
                  (:begin-line
                   (cond ((char= c #\Return)
                          (setf state :cr prev-state :key))
                         (t
                          (vector-push-extend c buf)
                          (setf state :key))))
                  (:key
                   (cond ((char= c #\:)
                          (setf k buf buf (make-buf) state :after-key))
                         (t
                          (vector-push-extend c buf))))
                  (:after-key
                   (cond ((char= c #\Space))
                         (t
                          (vector-push-extend c buf)
                          (setf state :value))))
                  (:value
                   (cond ((char= c #\Return)
                          (setf state :cr prev-state :value))
                         (t
                          (vector-push-extend c buf))))
                  (:cr
                   (cond ((char= c #\Newline)
                          (return))
                         (t
                          (vector-push-extend #\Return buf)
                          (vector-push-extend c buf)
                          (setf state prev-state prev-state nil))))))
      (when (< 0 (length k))
        (cons k buf)))))

(defun read-headers (stream)
  (loop :for kv := (read-header stream) :while kv :collect kv))

(defun write-crlf (stream)
  (write-char #\Return stream)
  (write-char #\Newline stream))

(defun write-header (stream kv)
  "Write a HTTP-header-formatted key value pair to STREAM."
  (write-string (car kv) stream)
  (write-char #\: stream)
  (write-char #\Space stream)
  (write-string (princ-to-string (cdr kv)) stream)
  (write-crlf stream))

(defun write-headers (stream kvs)
  (loop :for kv :in kvs :do (write-header stream kv))
  (write-crlf stream))

(defun get-header (map key)
  (cdr (assoc key map :test #'string-equal)))

(defun content-length (headers)
  (let ((value (get-header headers "Content-Length")))
    (when value
      (parse-integer value))))

(defclass rpc-message ()
  ((content :initarg :content
            :reader message-content
            :documentation "JSON text")
   (parsed-content :initarg :parsed
                   :initform nil)))

(defun parsed-content (rpc-message)
  (with-slots (content parsed-content) rpc-message
    (unless parsed-content
      (setf parsed-content (decode-json content)))
    parsed-content))

(defun message-field (rpc-message key)
  (cdr (assoc (camel-case key) (parsed-content rpc-message) :test #'string-equal)))

(defun message-id (rpc-message)
  (message-field rpc-message :id))

(defun message-type (rpc-message)
  (let ((id (message-field rpc-message :id))
        (method (message-field rpc-message :method)))
    (cond ((and method id)
           'request-message)
          ((and method (not id))
           'notification-message)
          ((and (not method) id)
           'response-message)
          (t
           nil))))

(defun trunc (n string)
  (if (< n (length string))
      (concatenate 'string (subseq string 0 (max 0 (- n 3))) "...")
      string))

(defmethod print-object ((self rpc-message) stream)
  (let* ((id (message-field self :id))
         (method (message-field self :method))
         (type (case (message-type self)
                 (request-message "request")
                 (notification-message "request")
                 (response-message "response")
                 (t "incomplete message"))))
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a ~a id: ~a value: ~s"
              type
              (or method "none")
              (or id "none")
              (trunc 16 (message-content self))))))

(defun read-rpc (stream)
  (let* ((headers (read-headers stream))
         (content-length (content-length headers))
         (content (make-array content-length :element-type 'character)))
    (read-sequence content stream :start 0 :end content-length)
    (let ((message (make-instance 'rpc-message
                     :content content)))
      (when (/trace-p)
        (/trace "rpc/read-rpc ~a" (reencode-json content)))
      message)))

(defun %write-rpc (message stream)
  (let ((content (message-content message)))
    (write-headers stream
                   `(("Content-Length" . ,(length content))
                     ("Content-Type" . "application/json-rpc")))
    (write-sequence content stream)
    (force-output stream)))

(defun write-rpc (content stream)
  (let ((message (make-instance 'rpc-message
                                :content content)))
    (when (/trace-p)
      (/trace "rpc/write-rpc~%~a" (reencode-json content)))
    (%write-rpc message stream)))
