;;;; clc8.asd

(asdf:defsystem #:clc8
  :description "Yet another CHIP-8 Interpreter"
  :author "Berat Ertural <beertural@gmail.com>"
  :license  "GNU GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria" "cl-opengl" "sdl2")
  :components ((:file "package")
	       (:module "src"
			:components
			((:file "clc8")
			 (:file "instructions")
			 (:file "graphics")
			 (:file "display" :depends-on ("graphics"))))))
