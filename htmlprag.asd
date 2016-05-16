(defsystem :cl-htmlprag
  :author ("Neil van Dyke")
  :maintainer "Jeremy Phelps"
  :description "A port of Neil Van Dyke's famous HTMLPrag library to Common Lisp."
  :version "0.24"
  :license "GPL"
  :depends-on (:optima :parse-number :alexandria)
  :components
  ((:file "read-macros")
   (:file "testeez" :depends-on ("schemish"))
   (:file "htmlprag" :depends-on ("schemish" "testeez"))
   (:file "schemish" :depends-on ("read-macros"))))
