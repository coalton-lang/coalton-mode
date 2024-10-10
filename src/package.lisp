(defpackage #:coalton-lsp
  (:documentation "An LSP server for the Coalton language")
  (:use #:cl)
  (:export #:main
           #:start
           #:stop
           #:restart))
