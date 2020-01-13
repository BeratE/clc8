;;;; clc8.lisp
(in-package #:clc8)

(defvar *chip* nil
  "Currently bound chip.")
(defvar *programpath* nil
  "Filepath to the last loaded program ROM.") 

(defconstant +screen-width+ 64)
(defconstant +screen-height+ 32)
(defconstant +memory-size+ #x1000)
(defconstant +program-address+ #x200)
(defconstant +font-address+ #x050)
(defconstant +font-data+ '(#xF0 #x90 #x90 #x90 #xF0    ; 0
			   #x20 #x60 #x20 #x20 #x70    ; 1
			   #xF0 #x10 #xF0 #x80 #xF0    ; 2
			   #xF0 #x10 #xF0 #x10 #xF0    ; 3
			   #x90 #x90 #xF0 #x10 #x10    ; 4
			   #xF0 #x80 #xF0 #x10 #xF0    ; 5
			   #xF0 #x80 #xF0 #x90 #xF0    ; 6
			   #xF0 #x10 #x20 #x40 #x40    ; 7
			   #xF0 #x90 #xF0 #x90 #xF0    ; 8
			   #xF0 #x90 #xF0 #x10 #xF0    ; 9
			   #xF0 #x90 #xF0 #x90 #x90    ; A
			   #xE0 #x90 #xE0 #x90 #xE0    ; B
			   #xF0 #x80 #x80 #x80 #xF0    ; C
			   #xE0 #x90 #x90 #x90 #xE0    ; D
			   #xF0 #x80 #xF0 #x80 #xF0    ; E
			   #xF0 #x80 #xF0 #x80 #x80))  ; F

(defstruct chip8
  "CHIP-8 Core module."
  (memory (make-array +memory-size+ :element-type '(unsigned-byte 8) :initial-element 0) :read-only t)
  (registers (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0) :read-only t)
  (stack (make-array 16 :element-type '(unsigned-byte 12) :fill-pointer 0) :read-only t)
  (program-counter +program-address+ :type (unsigned-byte 12))
  (index-register #x000 :type (unsigned-byte 12))
  (delay-timer #x00 :type (unsigned-byte 8))
  (sound-timer #x00 :type (unsigned-byte 8))
  (halt nil :type boolean))

;;; Macros
(defmacro with-chip (chip &body body)
  "Access all slots of the given chip as variables. Shortform of with-slots."
  `(with-slots ((memory memory)
		(registers registers)
		(stack stack)
		(program-counter program-counter)
		(index-register index-register)
		(delay-timer delay-timer)
		(sound-timer sound-timer)
		(halt halt))
       ,chip
     ,@body))


;;; Functions
(defun load-u16 (chip address)
  "Load a composite 16-bit value (big endian) from memory beginning at address."
  (let ((c #x00))
    (declare ((unsigned-byte 16) c))
    (with-slots ((memory memory)) chip
      (setf (ldb (byte 8 0) c) (aref memory (1+ address)))
      (setf (ldb (byte 8 8) c) (aref memory address)))
    c))

(defun fetch-instruction (chip)
  "Fetch the next instruction and increase the program counter."
  (with-slots ((program-counter program-counter)) chip
      (let ((instruction (load-u16 chip program-counter)))
	(incf program-counter 2)
	instruction)))

(defun load-rom (pathname)
  "Load the binaryfile in pathname and boot the chip."
  (let ((rom (alexandria:read-file-into-byte-vector pathname)))
    (when rom
      (setf *programpath* pathname)
      (boot rom))))

(defun boot (rom)
  "Load the given ROM bytevector into memory and power up the chip."
  (let ((chip (make-chip8)))
    (setf *chip* chip)
    (setf (subseq (chip8-memory chip) +font-address+) +font-data+)
    (setf (subseq (chip8-memory chip) +program-address+) rom)
    (run chip)))

(defun reset (chip)
  "Reset the chip stack, registers and program counter."
  (with-chip chip
    (setf program-counter +program-address+)
    (setf (fill-pointer stack) #x0)
    (setf delay-timer #x00)
    (setf sound-timer #x00)
    (setf halt nil)))

(defun run (chip)
  "Cycle through the instructions in memory until the chip is halted."
  (reset chip)
  (with-slots ((halt halt)) chip
    (do ((cycles 0 (1+ cycles)))
	(halt chip)
      (let ((instruction (fetch-instruction chip)))
	(execute chip instruction)))))
