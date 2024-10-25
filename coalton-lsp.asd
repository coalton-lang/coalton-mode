(in-package :asdf-user)

(defsystem #:coalton-lsp
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:coalton
               #:com.inuoe.jzon
               #:usocket)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               ;; 'lib' contains general purpose code
               (:module "lib"
                :serial t
                :components ((:file "log")
                             (:file "list")
                             (:file "name")
                             (:file "rpc")
                             (:file "process")
                             (:file "message")
                             (:file "json")
                             (:file "uri")))
               (:file "session")
               (:file "protocol")
               (:file "server")))

(defsystem #:coalton-lsp/tests
  :depends-on (#:coalton-lsp
               #:fiasco)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "mock")
               (:file "json-tests")
               (:file "lsp-tests")
               (:file "message-tests")
               (:file "protocol-tests")
               (:file "rpc-tests")
               (:file "session-tests")))
