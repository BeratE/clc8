;;;; Display rendering routines
(in-package #:clc8.display.graphics)

;;;
;;; Special variables
;;;

(defparameter *verbose* t "Print verbose output.")

(defstruct shader
  "Collection of rendering information required to draw to display."
  (program nil)
  (vertex-array nil)
  (vertex-data '(-1.0 1.0 0.0 -1.0 -1.0 0.0 1.0 -1.0 0.0 1.0 1.0 0.0))
  (index-data '(0 1 2 3 0 2)))
	   
;;;
;;; Macros
;;;

(defmacro with-vao (vao &body body)
  "Binds vertex array object and unbinds on block release."
  `(progn
     (gl:bind-vertex-array ,vao)
     ,@body
     (gl:bind-vertex-array 0)))

(defmacro with-shader-program (shader-program &body body)
  "Binds shader program and unbinds block on release."
  `(progn
     (gl:use-program ,shader-program)
     ,@body
     (gl:use-program 0)))

;;;
;;; Main Body
;;;

(defun init-gl ()
  "Initialize graphics library."
  (gl:disable :blend :depth-test)
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defun render (shader)
  "Single graphics update iteration"
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (with-shader-program (shader-program shader)
    (with-vao (shader-vertex-array shader)
      (%gl:draw-elements :triangles (length (shader-index-data shader)) :unsigned-int 0))))

(defun init-shader (shader)
  "Initialize shader object."
  (compile-shader-program shader)
  (init-buffers shader))

(defun delete-shader (shader)
  "Shutdown Graphics Library and clean up resources."
  (gl:delete-program (shader-program shader))
  (gl:delete-vertex-arrays (shader-vertex-array shader)))


(defun compile-shader-program (shader)
  "Compile shaderprogram with default vertex and fragment shader."
  (when *verbose*
    (format t "Compiling shader programm..~%"))
  (let ((vert-shader (gl:create-shader :vertex-shader))
	(frag-shader (gl:create-shader :fragment-shader)))
    (compile-shader vert-shader "~/common-lisp/clc8/src/shader/default.vert")
    (compile-shader frag-shader "~/common-lisp/clc8/src/shader/default.frag")
    (when (shader-program shader)
      (gl:delete-program (shader-program shader)))
    (setf (shader-program shader) (gl:create-program))
    (when (zerop (shader-program shader))
      (error "Failed to create shader program."))
    (gl:attach-shader (shader-program shader) vert-shader)
    (gl:attach-shader (shader-program shader) frag-shader)
    (gl:link-program (shader-program shader))
    (when *verbose*
      (format t "~A~%" (gl:get-program-info-log (shader-program shader))))
    (gl:delete-shader vert-shader)
    (gl:delete-shader frag-shader)))

(defun compile-shader (shader sourcefile)
  "Compile given shader with the given source."
  (when *verbose*
    (format t "Compiling shader source: ~A.~%" sourcefile))
  (let ((source (alexandria:read-file-into-string sourcefile)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (unless (gl:get-shader shader :compile-status)
      (error (format nil "Error compiling shader source file: ~A~%~A~%~A~%"
		     sourcefile source (gl:get-shader-info-log shader))))))

(defun init-buffers (shader)
  "Initialize vertex buffer and array objects"
  (when (shader-vertex-array shader)
    (gl:delete-vertex-arrays (shader-vertex-array shader)))
  (setf (shader-vertex-array shader) (gl:gen-vertex-array))
  (with-vao (shader-vertex-array shader)
    (let ((vertex-buffer (create-buffer-object (shader-vertex-data shader)
					       :type :float :target :array-buffer))
	  (index-buffer (create-buffer-object (shader-index-data shader)
					      :type :unsigned-int :target :element-array-buffer)))
      (gl:bind-buffer :array-buffer vertex-buffer)
      (gl:bind-buffer :element-array-buffer index-buffer)
      ;; Init VAO with metadata
      (let ((float-size (cffi:foreign-type-size :float)))
	(gl:vertex-attrib-pointer 0 3 :float :false (* 3 float-size) 0))
      (gl:enable-vertex-attrib-array 0))))


(defun create-buffer-object (data-sequence &key (type :float) (target :array-buffer) (usage :static-draw))
  "Creates a buffer object of the given data-sequence."
  (let ((array (alloc-gl-array-from-sequence data-sequence :type type))
	(buffer (gl:create-buffer)))
    (gl:bind-buffer target buffer)
    (gl:buffer-data target usage array)
    (gl:free-gl-array array)
    buffer))

(defun alloc-gl-array-from-sequence (data-sequence &key (type :float))
  "Allocates a gl:gl-array struct from the given sequence."
  (let ((arr (gl:make-null-gl-array type))
	(buf (cffi:foreign-alloc type :initial-contents data-sequence)))
    (setf (gl::gl-array-pointer arr) buf)
    (setf (gl::gl-array-size arr) (length data-sequence))
    arr))
