(in-package :asdf-user)

(defsystem "coalton-mode"
  :serial t
  :depends-on (#:usocket
               #:bordeaux-threads
               #:flexi-streams
               #:coalton)
  :components ((:file "coalton-mode")))
