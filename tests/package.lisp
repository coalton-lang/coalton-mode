(fiasco:define-test-package #:coalton-lsp/tests
  (:use
   #:cl)
  (:local-nicknames
   (#:lsp #:coalton-lsp)))

(in-package #:coalton-lsp/tests)

(defun is-string= (a b &optional (message "input"))
  "If strings A and B differ, signal a failure reporting the first position where this is true."
  (let ((compare-len (min (length a)
                          (length b))))
    (loop :for i :from 0 :below compare-len
          :unless (char= (aref a i)
                         (aref b i))
            :do (is (string= a b)
                    (format nil "Strings differ at offset ~A of ~A:~%A: ~A~%B: ~A"
                            i message a b))
                (return-from is-string=))
    (is (= (length a)
           (length b))
        (format nil "Strings differ at offset ~A of ~A:~%~A~%~A"
                compare-len message a b))))

(defun repository-path ()
  (let ((path (namestring (asdf:system-source-directory "coalton-lsp"))))
    (subseq path 0 (1- (length path)))))

(defun rpc-file (name)
  (let ((path (format nil "~a/tests/rpc/~a" (repository-path) name)))
    (unless (probe-file path)
      (error "RPC example input ~a not found at ~a" name path))
    path))

(defun pipe (input output &aux (buflen 8192))
  (let ((buf (make-array buflen :element-type 'character)))
    (loop
      :for bytes = (read-sequence buf input)
      :do (write-sequence buf output :start 0 :end bytes)
      :while (= bytes buflen))))

(defun read-file (filename)
  (with-output-to-string (output)
    (with-open-file (input filename :direction ':input)
      (pipe input output))))

(defun rpc-example (name)
  (make-instance 'lsp::rpc-message
    :content (read-file (rpc-file name))))

#+example (rpc-example "initialize.json")
