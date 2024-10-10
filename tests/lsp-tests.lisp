(in-package #:coalton-lsp/tests)

(deftest lsp-tests/initialize-result ()
  (let ((x (lsp::make-message 'lsp::initialize-result)))
    (lsp::set-field x (list :capabilities :position-encoding)
                    :utf32)
    (lsp::set-field x (list :server-info :name)
                    "Emacs")
    x))
