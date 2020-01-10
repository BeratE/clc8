;;;; clc8.lisp

(in-package #:clc8)

(defvar *chip* nil
  "Currently bound chip.")
(defvar *programpath* nil
  "Filepath to the last loaded program ROM.") 

(defconstant +memory-size+ #x1000)
(defconstant +font-address+ #x050)
(defconstant +program-address+ #x200)
(defconstant +font-data+ '(#xF0 #x90 #x90 #x90 #xF0 ; 0
			   #x20 #x60 #x20 #x20 #x70 ; 1
			   #xF0 #x10 #xF0 #x80 #xF0 ; 2
			   #xF0 #x10 #xF0 #x10 #xF0 ; 3
			   #x90 #x90 #xF0 #x10 #x10 ; 4
			   #xF0 #x80 #xF0 #x10 #xF0 ; 5
			   #xF0 #x80 #xF0 #x90 #xF0 ; 6
			   #xF0 #x10 #x20 #x40 #x40 ; 7
			   #xF0 #x90 #xF0 #x90 #xF0 ; 8
			   #xF0 #x90 #xF0 #x10 #xF0 ; 9
			   #xF0 #x90 #xF0 #x90 #x90 ; A
			   #xE0 #x90 #xE0 #x90 #xE0 ; B
			   #xF0 #x80 #x80 #x80 #xF0 ; C
			   #xE0 #x90 #x90 #x90 #xE0 ; D
			   #xF0 #x80 #xF0 #x80 #xF0 ; E
			   #xF0 #x80 #xF0 #x80 #x80)) ; F
(defconstant +screen-width+ 64)
(defconstant +screen-height+ 32)

(defstruct chip8
  "CHIP-8 Core module."
  (memory (make-array +memory-size+ :element-type '(unsigned-byte 8) :initial-element 0) :read-only t)
  (registers (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0) :read-only t)
  (stack (make-array 16 :element-type '(unsigned-byte 16) :fill-pointer 0) :read-only t)
  (program-counter +program-address+ :type (unsigned-byte 12))
  (index-register #x000 :type (unsigned-byte 12))
  (delay-timer #x00 :type (unsigned-byte 8))
  (sound-timer #x00 :type (unsigned-byte 8))
  (halt nil :type boolean))

(defmacro with-chip (chip &rest body)
  "Access slots of the given chip as variables."
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

(defmacro with-reg-xy (instruction &rest body)
  "Extract register location X and Y from instruction #x*XY*"
  `(let ((vx (aref registers (ldb (byte 4 8) ,instruction)))
	 (vy (aref registers (ldb (byte 4 4) ,instruction))))
     ,@body))

(defun load-u16 (chip address)
  "Load a 16-bit value (big endian) in the memory of the chip at the given address."
  (let ((c #x00))
    (declare ((unsigned-byte 16) c))
    (with-slots ((memory memory)) chip
      (setf (ldb (byte 8 0) c) (aref memory (1+ address)))
      (setf (ldb (byte 8 8) c) (aref memory address)))
    c))

(defun fetch-instruction (chip)
  "Fetch the next instruction the pc points to."
  (with-slots ((program-counter program-counter)) chip
      (let ((instruction (load-u16 chip program-counter)))
	(incf program-counter 2)
	instruction)))

(defun load-rom (pathname)
  "Load the Binaryfile in pathname and boot the chip."
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
  "Reset the chip stack and registers."
  (with-chip chip
    (setf program-counter +program-address+)
    (setf (fill-pointer stack) #x0)
    (setf delay-timer #x00)
    (setf sound-timer #x00)
    (setf halt nil)))

(defun run (chip)
  "Cycle through the instructions until the chip is halted."
  (reset chip)
  (with-chip chip
    (do ((cycles 0 (1+ cycles)))
	(halt chip)
      (let ((instruction (fetch-instruction chip)))
	(case (ldb (byte 4 12) instruction)
	  (#x0
	   (case (ldb (byte 8 0) instruction)
	     (#xFD       ; 00FD EXIT - Exit the interpreter
	      (setf halt t))
	     (#xE0       ; 00E0 CLS - Clear the display.
	      ) 
	     (#xEE       ; 00EE RET - Return from a subroutine.
	      (unless (= (fill-pointer stack) 0)
		(setf program-counter (vector-pop stack))))))
	  (#x1           ; 1nnn JP addr - Jump to location nnn.
	   (setf program-counter (ldb (byte 12 0) instruction)))
	  (#x2           ; 2nnn CALL addr - Call subroutine at nnn.
	   (when (vector-push program-counter stack)
	     (setf program-counter (ldb (byte 12 0) instruction))))
	  (#x3           ; 3xkk SE Vx, byte - Skip next instruction if Vx = kk.
	   (when (= (aref registers (ldb (byte 4 8) instruction))
		    (ldb (byte 8 0) instruction))
	     (incf program-counter 2)))
	  (#x4           ; 4xkk SNE Vx, byte - Skip next instruction if Vx != kk.
	   (unless (= (aref registers (ldb (byte 4 8) instruction))
		      (ldb (byte 8 0) instruction))
	     (incf program-counter 2)))
	  (#x5           ; 5xy0 SE Vx, Vy - Skip next instruction if Vx = Vy.
	   (with-reg-xy instruction
	     (when (= vx vy)
	       (incf program-counter 2))))
	  (#x6           ; 6xkk LD Vx, byte - Set Vx = kk.
	   (setf (aref registers (ldb (byte 4 8) instruction)) (ldb (byte 8 0) instruction)))
	  (#x7           ; 7xkk ADD Vx, byte - Set Vx = Vx + kk.
	   (incf (aref registers (ldb (byte 4 8) instruction)) (ldb (byte 8 0) instruction)))
	  (#x8
	   (case (ldb (byte 4 0) instruction)
	     (#x0        ;8xy0 LD Vx, Vy - Set Vx = Vy.
	      (with-reg-xy instruction
		(setf vx vy)))
	     (#x1        ; 8xy1 OR Vx, Vy - Set Vx = Vx OR Vy.
	      (with-reg-xy instruction
		(setf vx (logior vx vy))))
	     (#x2        ; 8xy2 AND Vx, Vy - Set Vx = Vx AND Vy.
	      (with-reg-xy instruction
		(setf vx (logand vx vy))))
	     (#x3        ; 8xy3 XOR Vx, Vy - Set Vx = Vx XOR Vy.
	      (with-reg-xy instruction
		(setf vx (logxor vx vy))))
	     (#x4        ; 8xy4 ADD Vx, Vy - Set Vx = Vx + Vy, set VF = carry.
	      (with-reg-xy instruction
		(let ((r (+ vx vy)))
		  (setf vx (ldb (byte 8 0) r))
		  (setf (aref registers #xF) (ldb (byte 1 8) r)))))
	     (#x5        ; 8xy5 SUB Vx, Vy - Set Vx = Vx - Vy, set VF = NOT borrow.
	      (with-reg-xy instruction
		(let ((r (- vx vy)))
		  (setf vx (ldb (byte 8 0) r))
		  (setf (aref registers #xF) (lognot (ldb (byte 1 8) r))))))
	     (#x6        ; 8xy6 SHR Vx {, Vy} - Set Vx = Vx SHR 1.
	      (let ((vx (aref registers (ldb (byte 4 8) instruction))))
		(setf (aref registers #xF) (ldb (byte 1 0) vx))
		(setf vx (ash vx -1))))
	     (#x7        ; 8xy7 SUBN Vx, Vy - Set Vx = Vy - Vx, set VF = NOT borrow.
	      (with-reg-xy instruction
		(let ((r (- vy vx)))
		  (setf vx (ldb (byte 8 0) r))
		  (setf (aref registers #xF) (lognot (ldb (byte 1 8) r))))))
	     (#xE        ; 8xyE SHL Vx {, Vy} - Set Vx = Vx SHL 1.
	      (let ((vx (aref registers (ldb (byte 4 8) instruction))))
		(setf (aref registers #xF) (ldb (byte 1 7) vx))
		(setf vx (ash vx 1))))))
	  (#x9           ; 9xy0 - SNE Vx, Vy - Skip next instruction if Vx != Vy.
	   (with-reg-xy instruction
	     (unless (= vx vy)
	       (incf program-counter 2))))
	  (#xA          ; Annn LD I, addr - Set I = nnn.
	   (setf index-register (ldb (byte 12 0) instruction)))
	  (#xB          ; Bnnn JP V0, addr - Jump to location nnn + V0.
	   (let ((v0 (aref registers #x0))
		 (nnn (ldb (byte 12 0) instruction)))
	     (setf program-counter (+ v0 nnn))))
	  (#xC          ; Cxkk - RND Vx, byte - Set Vx = random byte AND kk.
	   )
	  (#xD )
	  (#xE )
	  (#xF ))))))
