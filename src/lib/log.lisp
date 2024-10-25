;;;; This is a small library for emitting timestamped log messages, thread-safely.

(in-package :coalton-lsp)

(defun write-timestamp (stream)
  (multiple-value-bind (ss mm hh)
      (decode-universal-time (get-universal-time))
    (format stream "~2,'0d:~2,'0d:~2,'0d" hh mm ss)))

(defvar *context* nil)

(defun %add-context (context k f)
  (if (some (lambda (e)
              (eq k (car e)))
            context)
      context
      (nconc context (list (cons k f)))))

(defmacro with-logging-context ((k f) &body body)
  "Register function F under key K, if it has not been previously registered. F is called with a single stream-valued argument to add context information to all log messages emitted in dynamic scope "
  `(let ((*context* (%add-context *context* ,k ,f)))
     ,@body))

(defun %write-context (stream)
  "Helper for write-log: print any bound logging dynamic context messages."
  (dolist (c *context*)
    (funcall (cdr c) stream)
    (write-string " : " stream)))

(defgeneric log-p (destination level))

(defgeneric write-log (destination level format format-args metadata))

(defclass stream-logger ()
  ((stream :initarg :stream)
   (level :initform :info
          :initarg :level
          :reader log-level)
   (lock :initform (bt:make-lock))))

(defvar *levels*
  '(:trace :debug :info :warn :error))

(defun level<= (a b)
  (<= (position a *levels*)
      (position b *levels*)))

(defmethod log-p ((self stream-logger) level)
  (level<= (log-level self) level))

(defmethod write-log ((self stream-logger) level format format-args metadata)
  (when (log-p self level)
    (with-slots (stream lock) self
      (bt:with-lock-held (lock)
        (write-string ";; " stream)
        (write-timestamp stream)
        (write-string " : " stream)
        (princ level stream)
        (write-string " : " stream)
        (%write-context stream)
        (apply #'format stream format format-args)
        (terpri stream)))))

(defparameter *logger*
  (make-instance 'stream-logger :stream t :level ':info))

(defun %log (level message args)
  (write-log *logger* level message args nil)
  (values))

(defun set-log-level (level)
  (setf (slot-value *logger* 'level) level))

(defun /trace-p ()
  (log-p *logger* ':trace))

(defun /trace (message &rest args)
  (%log :trace message args))

(defun /debug-p ()
  (log-p *logger* ':debug))

(defun /debug (message &rest args)
  (%log :debug message args))

(defun /info (message &rest args)
  (%log :info message args))

(defun /warn (message &rest args)
  (%log :warn message args))

(defun /error (message &rest args)
  (%log :error message args))
