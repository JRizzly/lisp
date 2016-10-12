;;;; cc-lawn.asd

(asdf:defsystem #:cc-lawn
  :serial t
  :description "Lawn In Order"
  :author "Matthew Carter <m@ahungry.com>"
  :license "Copyright Ahungry 2014"
  :depends-on (#:hunchentoot
               #:cl-who
               #:css-lite
               #:parenscript
               #:glyphs)
  :components ((:file "package")
               (:file "css")
               (:file "js")
               (:file "cc-lawn")))

