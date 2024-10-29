(defpackage #:coalton-lsp
  (:documentation "An LSP server for the Coalton language")
  (:use #:cl)
  (:export #:main
           #:*server*
           #:*server-port*
           #:set-log-level
           #:set-log-file
           #:start-server
           #:stop-server))
