(in-package #:coalton-lsp/tests)

(deftest json-tests/decode ()

  (is (equalp (lsp::decode-json
               "{\"key\": \"value\"}")
              '(("key" . "value")))))
