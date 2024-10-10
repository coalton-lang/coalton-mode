;;;; Behavior common to classes that manage processes

(in-package :coalton-lsp)

(defgeneric start (process)
  (:documentation "Start a process. The started process is returned."))

(defgeneric name (process)
  (:documentation "Process name.")
  (:method (process) "Coalton LSP Process"))

(defgeneric run (process)
  (:documentation "Run a process. This function is run by a newly started process. IF the function returns, the process will halt."))

(defgeneric stop (process)
  (:documentation "Synchronously stop a process and immediately return."))

(defclass process ()
  ((thread :initform nil)
   (lock :initform (bt:make-recursive-lock))))

(defmacro with-lock-held ((process) &body scope)
  `(bt:with-recursive-lock-held ((slot-value ,process 'lock))
     ,@scope))

(defmethod start ((self process))
  (with-slots (thread) self
    (setf thread (bt:make-thread (lambda ()
                                   (run self))
                                 :name (name self))))
  self)

(defmethod stop ((self process))
  (with-slots (thread) self
    (when thread
      (bt:destroy-thread thread)
      (setf thread nil)))
  self)

;;; Work queue

(defvar *worker-poll-interval* 0.250
  "How long to sleep when there is no work to do.")

(defclass worker (process)
  ((fn)
   (run :initform t
        :reader run-p)
   (queue :initform nil)))

(defun enqueue (worker element)
  (with-lock-held (worker)
    (with-slots (queue) worker
      (setf queue (nconc queue (list element))))))

(defun dequeue (worker)
  (with-lock-held (worker)
    (with-slots (queue) worker
      (let ((element (car queue)))
        (setf queue (cdr queue))
        element))))

(defun empty-p (worker)
  (with-lock-held (worker)
    (null (slot-value worker 'queue))))

(defun service-queue (worker)
  (loop :while (and (run-p worker)
                    (not (empty-p worker)))
        :do (let ((element (dequeue worker)))
              (/debug "about to process one entry")
              (handler-case
                  (funcall (slot-value worker 'fn) element)
                (condition (condition)
                  (break)
                  (/error "ignoring error : ~a" condition))))))

(defmethod run ((self worker))
  (with-logging-context (:worker (lambda (stream)
                                   (write-string "worker" stream)))
    (/debug "starting")
    (unwind-protect
         (loop :while (run-p self)
               :do (service-queue self)
                   (sleep *worker-poll-interval*))
      (/debug "exiting"))))
