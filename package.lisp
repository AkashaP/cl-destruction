(defpackage #:cl-destruction
  (:use #:cl
        #:alexandria
        #:serapeum)
  (:export
   #:nrotate-left 
   #:nrotate-right 
   #:nsync 
   #:ucopy 
   #:nupdate
   #:bubblesort
   #:bubblesync
   #:delete-prop
   #:set-prop
   #:get-prop
   #:atomp))
(in-package :cl-destruction)
