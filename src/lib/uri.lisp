;;;; Just enough to parse simple file: scheme URLs

(defpackage #:lib.uri
  (:use
   #:cl)
  (:export
   #:parse
   #:input-stream))

(in-package #:lib.uri)

(defstruct uri
  scheme
  path)

(defun parse (string)
  (cond ((alexandria:starts-with-subseq "file:" string)
         (make-uri :scheme "file"
                   :path (subseq string 7)))
        (t
         nil)))

(defvar *stream-providers*
  (make-hash-table :test #'equalp))

(defstruct stream-provider
  input
  output)

(defun define-stream-provider (name ignore in out)
  (declare (ignore ignore))
  (setf (gethash name *stream-providers*)
        (make-stream-provider :input in
                              :output out)))

(define-stream-provider "file" ()
  (lambda (uri)
    (open (uri-path uri)
          :direction :input
          :element-type 'character))
  (lambda (uri)
    (open (uri-path uri)
          :direction :output
          :element-type 'character)))

(defun stream-provider (url)
  (or (gethash (uri-scheme url) *stream-providers*)
      (error "No stream provider for scheme ~A" (uri-scheme url))))

(defun input-stream (url)
  (funcall (stream-provider-input (stream-provider url)) url))
