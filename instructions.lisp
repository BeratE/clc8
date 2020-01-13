;;;; Implementation of the CHIP-8 instruction set
(in-package #:clc8)


;;; Helper Functions
(defun nibble-x (instruction)
  "Extract x nibble from the instruction."
  (ldb (byte 4 8) instruction))

(defun nibble-y (instruction)
  "Extract y nibble from the instruction."
  (ldb (byte 4 4) instruction))

(defun byte-kk (instruction)
  "Extract byte kk from the instruction."
  (ldb (byte 8 0) instruction))


;;; Macros
(defmacro with-reg-vx (instruction &body body)
  "Extract register index X from instruction #x*X**."
  `(let ((vx (nibble-x ,instruction)))
     ,@body))

(defmacro with-reg-vx-vy (instruction &body body)
  "Extract register index X and Y from instruction #x*XY*."
  `(let ((vx (nibble-x ,instruction))
	 (vy (nibble-y ,instruction)))
     ,@body))


;;; Main
(defun execute (chip instruction)
  "Execute a single two byte instruction of the CHIP-8 instruction set."
  (with-chip chip
    (case (ldb (byte 4 12) instruction)
      (#x0 (case (ldb (byte 8 0) instruction)
	     ; 00FD EXIT - Exit the interpreter
	     (#xFD (setf halt t))
	     ; 00E0 CLS - Clear the display.
	     (#xE0  ) ;; TODO
	     ; 00EE RET - Return from a subroutine.
	     (#xEE (unless (= (fill-pointer stack) 0)
		     (setf program-counter (vector-pop stack))))))
      ; 1nnn JP addr - Jump to location nnn.
      (#x1 (setf program-counter (ldb (byte 12 0) instruction)))
      ; 2nnn CALL addr - Call subroutine at nnn.
      (#x2 (when (vector-push program-counter stack)
	     (setf program-counter (ldb (byte 12 0) instruction))))
      ; 3xkk SE Vx, byte - Skip next instruction if Vx = kk.
      (#x3 (with-reg-vx instruction
	     (when (= (elt registers vx) (byte-kk instruction))
	       (incf program-counter 2))))
      ; 4xkk SNE Vx, byte - Skip next instruction if Vx != kk.
      (#x4 (with-reg-vx instruction
	     (unless (= (elt registers vx) (byte-kk instruction))
	       (incf program-counter 2))))
      ; 5xy0 SE Vx, Vy - Skip next instruction if Vx = Vy.
      (#x5 (with-reg-vx-vy instruction
	     (when (= (elt registers vx) (elt registers vy))
	       (incf program-counter 2))))
      ; 6xkk LD Vx, byte - Set Vx = kk.
      (#x6 (with-reg-vx instruction
	     (setf (elt registers vx) (byte-kk instruction))))
      ; 7xkk ADD Vx, byte - Set Vx = Vx + kk.
      (#x7 (with-reg-vx instruction
	     (incf (elt registers vx) (byte-kk instruction))))
      (#x8 (case (ldb (byte 4 0) instruction)
	     ;8xy0 LD Vx, Vy - Set Vx = Vy.
	     (#x0 (with-reg-vx-vy instruction
		    (setf (elt registers vx) (elt registers vy))))
	     ; 8xy1 OR Vx, Vy - Set Vx = Vx OR Vy.
	     (#x1 (with-reg-vx-vy instruction
		    (setf (elt registers vx) (logior (elt registers vx) (elt registers vy)))))
	     ; 8xy2 AND Vx, Vy - Set Vx = Vx AND Vy.
	     (#x2 (with-reg-vx-vy instruction
		    (setf (elt registers vx) (logand (elt registers vx) (elt registers vy)))))
	     ; 8xy3 XOR Vx, Vy - Set Vx = Vx XOR Vy.
	     (#x3 (with-reg-vx-vy instruction
     		    (setf (elt registers vx) (logxor (elt registers vx) (elt registers vy)))))
	     ; 8xy4 ADD Vx, Vy - Set Vx = Vx + Vy, set VF = carry.
	     (#x4 (with-reg-vx-vy instruction
		    (let ((r (+ (elt registers vx) (elt registers vy))))
		      (setf (elt registers vx) (ldb (byte 8 0) r))
		      (setf (elt registers #xF) (ldb (byte 1 8) r)))))
	     ; 8xy5 SUB Vx, Vy - Set Vx = Vx - Vy, set VF = NOT borrow.
	     (#x5 (with-reg-vx-vy instruction  
		    (let ((r (- (elt registers vx) (elt registers vy))))
		      (setf (elt registers vx) (ldb (byte 8 0) r))
		      (setf (elt registers #xF) (lognot (ldb (byte 1 8) r))))))
	     ; 8xy6 SHR Vx {, Vy} - Set Vx = Vx SHR 1.
	     (#x6 (with-reg-vx instruction
	  	    (setf (elt registers #xF) (ldb (byte 1 0) (elt registers vx)))
		    (setf (elt registers vx) (ash (elt registers vx) -1))))
	     ; 8xy7 SUBN Vx, Vy - Set Vx = Vy - Vx, set VF = NOT borrow.
	     (#x7 (with-reg-vx-vy instruction
		    (let ((r (- (elt registers vy) (elt registers vx))))
		      (setf (elt registers vx) (ldb (byte 8 0) r))
		      (setf (elt registers #xF) (lognot (ldb (byte 1 8) r))))))
	     ; 8xyE SHL Vx {, Vy} - Set Vx = Vx SHL 1.
	     (#xE (with-reg-vx instruction
		    (setf (elt registers #xF) (ldb (byte 1 7) (elt registers vx)))
		    (setf (elt registers vx) (ash (elt registers vx) 1))))))
      ; 9xy0 - SNE Vx, Vy - Skip next instruction if Vx != Vy.
      (#x9 (with-reg-vx-vy instruction
	     (unless (= (elt registers vx) (elt registers vy))
	       (incf program-counter 2))))
      ; Annn LD I, addr - Set I = nnn.
      (#xA (setf index-register (ldb (byte 12 0) instruction)))
      ; Bnnn JP V0, addr - Jump to location nnn + V0.
      (#xB (let ((v0 (aref registers #x0))
		 (nnn (ldb (byte 12 0) instruction)))
	     (setf program-counter (+ v0 nnn))))
      ; Cxkk RND Vx, byte - Set Vx = random byte AND kk.
      (#xC (with-reg-vx instruction
	     (setf (elt registers vx) (logand (random 256) (byte-kk instruction)))))
      ; Dxyn DRW Vx, Vy, nibble - Draw n-byte sprite in address I at (Vx, Vy), VF = collision.
      (#xD )        ;; TODO
      (#xE (case (ldb (byte 8 0) instruction)
	     ; Ex9E SKP Vx - Skip next instruction if key with the value of Vx is pressed.
	     (#x9E )    ;; TODO
	     ; ExA1  SKNP Vx - Skip next instruction if key with the value of Vx is not pressed.
	     (#xA1 )))  ;; TODO
      (#xF (case (ldb (byte 8 0) instruction)
	     ; Fx07 LD Vx, DT Set Vx = delay timer value.
	     (#x07 (with-reg-vx instruction
		     (setf (elt registers vx) delay-timer)))
	     ; Fx0A LD Vx, K - Wait for a key press, store the value of the key in Vx.
	     (#x0A )    ;; TODO
	     ; Fx15 LD DT, Vx - Set delay timer = Vx.
	     (#x15 (with-reg-vx instruction
		     (setf delay-timer (elt registers vx))))
	     ; Fx18 LD ST, Vx - Set sound timer = Vx.
	     (#x18 (with-reg-vx  instruction
		     (setf sound-timer (elt registers vx))))
	     ; Fx1E ADD I, Vx - Set I = I + Vx.
	     (#x1E (with-reg-vx instruction
		     (incf index-register (elt registers vx))))
	     ; Fx29 - LD F, Vx Set I = location of sprite for digit Vx.
	     (#x29 (with-reg-vx instruction
		     (setf index-register (+ +font-address+ (* (elt registers vx) 5) ))))
	     ; Fx33 LD B, Vx - Store BCD of Vx in memory I, I+1, I+2.
	     (#x33 (with-reg-vx instruction
		     (let ((n (elt registers vx)))
		       (setf (aref memory (+ index-register 0)) (truncate (/ n 100)))
		       (setf (aref memory (+ index-register 1)) (truncate (/ (mod n 100) 10)))
		       (setf (aref memory (+ index-register 2)) (mod (mod n 100) 10)))))
	     ; Fx55 LD [I], Vx - Store registers V0 through Vx in memory starting at location I.
	     (#x55 (replace memory registers :start1 index-register :end2 (nibble-x instruction)))
	     ; Fx65 LD Vx, [I] - Read registers V0 through Vx from memory starting at location I.
	     (#x65 (replace registers memory :start2 index-register :end1 (nibble-x instruction))))))))
  
	  

