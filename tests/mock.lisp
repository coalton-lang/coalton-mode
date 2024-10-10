(in-package :coalton-lsp/tests)

(defun mock-publish-diagnostics ()
  (lsp::make-notification
   "textDocument/publishDiagnostics"
   (let ((message (lsp::make-message 'lsp::text-document-publish-diagnostics-params)))
     (lsp::set-field message :uri "file:///Users/jlbouwman/git/coalton-mode/resources/fib.coal")
     (lsp::set-field message :diagnostics
                     (list (lsp::message-value
                            (lsp::make-diagnostic 4 4 4 7
                                                  "export: 'fob' is undefined"
                                                  "undefined-export"))))
     message)))
