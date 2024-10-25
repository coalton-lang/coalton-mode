(in-package #:coalton-lsp/tests)

(deftest protocol-tests/initialize ()
  (let ((params (lsp::request-params (lsp::make-request (rpc-example "initialize.json")))))
    (lsp::get-field params :root-uri)
    (lsp::get-field params :workspace-folders)))
