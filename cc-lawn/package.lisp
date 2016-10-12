;;;; package.lisp

(defpackage #:cc-lawn
  (:use #:cl
        #:parenscript
        #:cl-who
        #:css-lite
        #:hunchentoot
        #:glyphs)
  (:shadowing-import-from #:parenscript #:get-time #:%)
  (:export #:main))
