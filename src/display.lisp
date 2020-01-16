;;;; The display system of the CHIP-8 Emulator
(in-package #:clc8.display)

;;;
;;; Constants
;;;

(defconstant +bg-r+ 15 "Backgroundcolor red")
(defconstant +bg-g+ 56 "Backgroundcolor green")
(defconstant +bg-b+ 15 "Backgroundcolor blue")
(defconstant +fg-r+ 139 "Foregroundcolor red")
(defconstant +fg-g+ 172 "Foregroundcolor green")
(defconstant +fg-b+ 15 "Foregroundcolor blue")
(defconstant +pixel-size+ 8 "Size of a pixel")
(defconstant +win-width+ (* 64 +pixel-size+) "Display window width")
(defconstant +win-height+ (* 32 +pixel-size+) "Display window height")

;;;
;;; Special Variables
;;;

(defparameter *verbose* t "Print verbose output")

;;;
;;; Macros
;;;

(defmacro continuable (&body body)
  "Allow continuing execution from errors."
  `(restart-case (progn ,@body)
     (continue () :report "Continue")))

;;;
;;; Helper functions
;;;

(defun update-swank ()
  "Handle REPL requests."
  #+swank
  (continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))

;;;
;;; Main body 
;;;

(defun print-display-info ()
  (format t "OpenGL Version ~D.~D ~A~%SDL Library Version ~D.~D.~D~%"
	  (sdl2:gl-get-attr :context-major-version)
	  (sdl2:gl-get-attr :context-minor-version)
	  (sdl2:gl-get-attr :context-profile-mask)
	  sdl2-ffi:+sdl-major-version+
	  sdl2-ffi:+sdl-minor-version+
	  sdl2-ffi:+sdl-patchlevel+)
  (finish-output))

  
(defun display ()
  "Start a display system."
  (sdl2:with-init (:everything)
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)
    (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
    (when *verbose* (print-display-info))
    (sdl2:with-window (window :title "Common Lisp CHIP-8"
			      :w +win-width+ :h +win-height+
			      :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context window)
	(init-gl)
	(let ((shader (make-shader)))
	  (init-shader shader)
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup (:keysym keysym)
		    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
		      (sdl2:push-event :quit)))
	    (:idle ()
		   (update-swank)
		   (continuable
		     (render shader)
		     (sdl2:gl-swap-window window)))
	    (:quit () t))
	  (delete-shader shader))))))
