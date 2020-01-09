;;;; clc8.lisp

(in-package #:clc8)

(defconstant +memory-size+ (* 4 1024))
(defconstant +program-address+ #x000)

(defstruct chip8
  "CHIP-8 Core module."
  (memory (make-array +memory-size+ :element-type '(unsigned-byte 8) :initial-element 0) :read-only t)
  (registers (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0) :read-only t)
  (stack (make-array 16 :element-type '(unsigned-byte 16) :fill-pointer 0) :read-only t)
  (program-counter +program-address+ :type (unsigned-byte 12))
  (i +program-address+ :type (unsigned-byte 12))
  ;; Special Purpose Registers
  (delay-timer #x00 :type (unsigned-byte 8))
  (sound-timer #x00 :type (unsigned-byte 8)))

