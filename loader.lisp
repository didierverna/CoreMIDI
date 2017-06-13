;; #### FIXME: ASDF probably has a more declarative way of doing this kind of
;; thing

#-ccl
(handler-case
    (let* ((dir (concatenate 'string
		  (namestring (asdf:system-source-directory :coremidi))
		  "ObjectiveC/"))
	   (file (concatenate 'string dir "libwrapper.dylib")))
      (unless (probe-file file)
	(uiop:run-program (concatenate 'string "make -C " dir) :output t))
      (cffi:load-foreign-library file))
  (error ())) ;; #### FIXME: WTF?

#+ccl
(progn
  (cffi:define-foreign-library coremidi (:darwin (:framework "CoreMIDI")))
  (cffi:use-foreign-library coremidi))
