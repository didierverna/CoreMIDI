(in-package :coremidi)

(defmacro while (test &rest body)
  `(do () ((not ,test))
     ,@body))

(defconstant +k-cf-string-encoding-utf-8+ #x08000100
  "The kCFStringEncodingUTF8 constant.")

(defun cf-string (string)
  "Create a UTF8 CoreFoundation string out of STRING.
This string needs to be released after use."
  (cffi:foreign-funcall "CFStringCreateWithCString"
    :pointer (cffi:foreign-funcall "CFAllocatorGetDefault" :pointer)
    :string string
    :int +k-cf-string-encoding-utf-8+
    :pointer))

(defun cf-release (cf-object)
  "Release CF-OBJECT."
  (cffi:foreign-funcall "CFRelease" :pointer cf-object))

(defmacro with-cf-strings (bindings &body body)
  "Execute BODY with a set of BINDINGS to CoreFoundation strings.
Each binding looks like (VAR STRING). every VAR will be bound to a newly
created CoreFoundation string initialized from plain STRING. Those strings
will be released afterwards."
  `(let ,(mapcar #'car bindings)
     ;; #### FIXME: we can do better in terms of error handling.
     (unwind-protect
	  (progn
	    (setq ,@(loop :for form :in bindings
			  :nconc `(,(car form) (cf-string ,(cadr form)))))
	    ,@body)
       ,@(loop :for form :in (reverse bindings)
	       :collect `(cf-release ,(car form))))))
