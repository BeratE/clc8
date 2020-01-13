;;;; package.lisp

(defpackage #:clc8
  (:use #:cl)
  (:export :load-rom
	   :boot
	   :reset
	   :run
	   :+memory-size+
	   :+font-address+
	   :+font-data+
	   :+program-address+
	   :chip8
	   :chip8-memory
	   :chip8-registers
	   :chip8-stack
	   :chip8-program-counter
	   :chip8-index-register
	   :chip8-delay-timer
	   :chip8-sound-timer
	   :chip8-halt
	   :with-chip))
