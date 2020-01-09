;;;; clc8.asd

(asdf:defsystem #:clc8
  :description "Yet another CHIP-8 Interpreter"
  :author "Berat Ertural <beertural@gmail.com>"
  :license  "GNU GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria")
  :components ((:file "package")
               (:file "clc8")))
