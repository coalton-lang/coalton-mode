;;;; Functions related to names and identifiers

(in-package #:coalton-lsp)

(defun camel-case (keyword)
  "Convert hyphen-delimited keyword identifiers to camel-cased strings."
  (with-output-to-string (stream)
    (loop :with upper
          :for char :across (symbol-name keyword) :while char
          :if (char= char #\-)
            :do (setf upper t)
          :else
            :do (write-char (if upper char (char-downcase char)) stream)
                (setf upper nil))))

#+example

(camel-case :some-thing)

;; => "someThing"
