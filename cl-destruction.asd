(in-package #:cl-user)

(asdf:defsystem #:cl-destruction
  :serial t
  :components ((:file "package")
               (:file "destruction"))
  :depends-on (#:alexandria
               #:serapeum))
