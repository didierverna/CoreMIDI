(in-package :coremidi)

;; ==========================================================================
;; MIDI Messages
;; ==========================================================================

(defconstant +note-off+                #x80)
(defconstant +note-on+                 #x90)
(defconstant +polyphonic-aftertouch+   #xA0)
(defconstant +control/mode-change+     #xB0)
(defconstant +program-change+          #xC0)
(defconstant +channel-aftertouch+      #xD0)
(defconstant +pitch-wheel-range+       #xE0)
(defconstant +system-exclusive+        #xF0)
;; System Common
;;           Undefined                 #xF1
(defconstant +song-position-pointer+   #xF2)
(defconstant +song-select+             #xF3)
;;           Undefined                 #xF4
;;           Undefined                 #xF5
(defconstant +tune-request+            #xF6)
(defconstant +end-of-system-exclusive+ #xF7)
;; System Real Time
(defconstant +timing-clock+            #xF8)
;;           Undefined                 #xF9
(defconstant +start+                   #xFA)
(defconstant +continue+                #xFB)
(defconstant +stop+                    #xFC)
;;           Undefined                 #xFD
(defconstant +active-sensing+          #xFE)
(defconstant +system-reset+            #xFF)


(defun display-name (object)
  "Return OBJECT's display name property."
  (object-string-property object "displayName"))

(defun all-endpoints (direction)
  (multiple-value-bind (number-f get-f)
      (ecase direction
	(:input (values #'get-number-of-sources #'get-source))
	(:output (values #'get-number-of-destinations #'get-destination)))
    (loop for i from 0 below (funcall number-f)
	  collect (funcall get-f i))))

(defun list-of-sources ()
  "print the name of sources in system."
  (let ((all-sources (all-endpoints :input)))
    (loop for src in all-sources
	  for i from 0
	  do (format t "~2d:  ~a~%" i (display-name src)))))

(defun list-of-destinations ()
  "print the name of destinations in system."
  (let ((all-dest (all-endpoints :output)))
    (loop for dst in all-dest
	  for i from 0
	  do (format t "~2d:  ~a~%" i (display-name dst)))))


(defun find-source (name)
  "Returns a source which has a same name."
  (find name (all-endpoints :input)
	:test #'string= :key #'display-name))

(defun find-destination (name)
  "Returns a destination which has a same name."
  (find name (all-endpoints :output)
	:test #'string= :key #'display-name))


(defvar *midi-client* nil
  "The current MIDI client as a property-list.
Properties are:
:client
:in-port and :out-port
:sources
:handlers
:virtual-endpoints")

(defun client-handlers (&optional (client *midi-client*))
  "Return CLIENT's handlers."
  (getf client :handlers))
(defun (setf client-handlers)
    (handlers &optional (client *midi-client*))
  "Set CLIENT's handlers."
  (setf (getf client :handlers) handlers))


;; ==========================================================================
;; Incoming MIDI Processing
;; ==========================================================================

(defun handle-packet
    (packet source
     &aux (handlers (cdr (assoc source (client-handlers))))
       (i 0))
  "Handle PACKET coming from SOURCE.
Each MIDI message in PACKET is dispatched to the appropriate handler, if one
is installed."
  (unless handlers
    (cffi:with-foreign-slots ((length data) packet (:struct packet))
      (handler-case
	  (while (< i length)
	    (let ((status  (cffi:mem-aref data :unsigned-char i)))
	      (cond
		;; Note On, Note Off and Polyphonic Aftertouch
		;; #### NOTE: contrary to the original code, I don't it's
		;; right to translate a Note On message with velocity 0 to a
		;; Note Off one here. We're still too low level. Rather, it
		;; should be the job of the handler to do so.
		((and (>= status +note-off+) (< status +control/mode-change+))
		 (alexandria:when-let*
		     ((message (logand status #xf0))
		      (handler (getf handlers message))
		      (channel (1+ (logand status #x0f)))
		      (data1 (cffi:mem-aref data :unsigned-char (+ i 1)))
		      (data2 (cffi:mem-aref data :unsigned-char (+ i 2))))
		   (funcall handler channel data1 data2))
		 (incf i 3))
		;; Control/Mode Change
		((and (>= status +control/mode-change+)
		      (< status +program-change+))
		 (let* ((number (cffi:mem-aref data :unsigned-char (+ i 1)))
			(value (if (or (< number #x60)
				       (= number #x7A)
				       ;; #### FIXME: not sure about this one
				       (= number #x7E))
				   (cffi:mem-aref data :unsigned-char (+ i 2))
				   'unused)))
		   (alexandria:when-let
		       ((handler (getf handlers +control/mode-change+))
			(channel (1+ (logand status #x0f))))
		     (funcall handler channel number value))
		   (incf i (if (eq value 'unused) 2 3))))
		;; Program Change and Channel Aftertouch
		;; Almost the same as Note On, Note Off and Polyphonic
		;; Aftertouch, except that there is no data2.
		((and (>= status +program-change+)
		      (< status +pitch-wheel-range+))
		 (alexandria:when-let*
		     ((message (logand status #xf0))
		      (handler (getf handlers message))
		      (channel (1+ (logand status #x0f)))
		      (data1 (cffi:mem-aref data :unsigned-char (+ i 1))))
		   (funcall handler channel data1))
		 (incf i 2))
		;; Pitch Wheel Range
		((and (>= status +pitch-wheel-range+)
		      (< status +system-exclusive+))
		 (alexandria:when-let
		     ((handler (getf handlers +pitch-wheel-range+))
		      (channel (1+ (logand status #x0f)))
		      (lsb (cffi:mem-aref data :unsigned-char (+ i 1)))
		      (msb (cffi:mem-aref data :unsigned-char (+ i 2))))
		   (funcall handler channel (logior (ash msb 7) lsb)))
		 (incf i 3))
		;; System Exclusive
		((= status +system-exclusive+)
		 ;; #### FIXME: implement.
		 )
		;; Song Position Pointer
		((= status +song-position-pointer+)
		 (alexandria:when-let
		     ((handler (getf handlers +song-position-pointer+))
		      (lsb (cffi:mem-aref data :unsigned-char (+ i 1)))
		      (msb (cffi:mem-aref data :unsigned-char (+ i 2))))
		   (funcall handler (logior (ash msb 7) lsb)))
		 (incf i 3))
		;; Song Select
		((= status +song-select+)
		 (alexandria:when-let
		     ((handler (getf handlers +song-select+))
		      (data1 (cffi:mem-aref data :unsigned-char (+ i 1))))
		   (funcall handler data1))
		 (incf i 2))
		;; The rest
		;; #### NOTE: some statuses in the range below are undefined,
		;; but it doesn't really matter since we wouldn't have a
		;; handler for them anyway.
		((and (>= status +tune-request+) (<= status +system-reset+))
		 (alexandria:when-let ((handler (getf handlers status)))
		   (funcall handler))
		 (incf i)))))
	(error (c) (format t "Error while processing packet: ~A~%" c))))))


;;; CCL has FOREIGN-THREAD-CALLBACK, so we can provide the callback function
;;; below directly to the system. In other implementations, we need a C
;;; wrapper.
#+ccl
(cffi:defcallback handle-packets :void ((pktlist :pointer)
					(read-proc-ref-con :pointer)
					(src-conn-ref-con :pointer))
  (declare (ignore read-proc-ref-con))
  (let ((packets-number (cffi:foreign-slot-value
			 pktlist '(:struct packet-list) 'num-packets))
	(packet (cffi:foreign-slot-pointer
		 pktlist '(:struct packet-list) 'packet)))
    (loop :for i :from 0 :below packets-number
	  :do (handle-packet packet
			      (cffi-sys:pointer-address src-conn-ref-con))
	      (setf packet (cffi-sys:inc-pointer
			    (cffi:foreign-slot-pointer packet '(:struct packet)
						       'data)
			    (cffi:foreign-slot-value packet '(:struct packet)
						     'length))))))

#-ccl
(let (thread)
  (defun create-packets-handler-thread ()
    (unless thread
      (setf thread
	    (bt:make-thread
	     (lambda ()
	       (let ((lock (cffi:foreign-symbol-pointer "packets"))
		     (ready (cffi:foreign-symbol-pointer "packet_ready"))
		     (handled (cffi:foreign-symbol-pointer "packet_handled"))
		     (flag (cffi:foreign-symbol-pointer "packetFlag"))
		     (packet (cffi:foreign-symbol-pointer "packet"))
		     (endpoint (cffi:foreign-symbol-pointer "packetEndpoint")))
		 (cffi:foreign-funcall "pthread_mutex_lock" :pointer lock)
		 (loop
		   (cffi:foreign-funcall "pthread_cond_wait"
					 :pointer ready :pointer lock)
		   (let ((flag-value (cffi:mem-ref flag :int)))
		     (assert (or (zerop flag-value) (= 1 flag-value))
			     nil
			     "Packets handler thread out of sync.")
		     (when (= 1 flag-value)
		       (decf (cffi:mem-ref flag :int))
		       (handle-packet (cffi:mem-ref packet :pointer)
				      (cffi:mem-ref endpoint 'object-ref))))
		   (cffi:foreign-funcall "pthread_cond_signal"
					 :pointer handled))))
	     :name "Packets handler")))))

(defun register-message-handler
    (source message handler &optional (client *midi-client*)
     &aux (client-handlers (client-handlers client))
       (source-handlers (assoc source client-handlers)))
  "Register HANDLER to process MIDI MESSAGEs coming from SOURCE."
  (unless source-handlers
    (port-connect-source
     (getf client :in-port) source (cffi-sys:make-pointer source))
    (pushnew source (getf client :sources))
    (setf source-handlers (list source)
	  (client-handlers client) (cons source-handlers client-handlers)))
  (setf (getf (cdr source-handlers) message) handler))


;;; send-midi-message to destination
(defun send-midi-message (destination timestamp status data1 data2)
  (let (data-length bytes)
    (if data2
	(setq data-length 3
	      bytes (list status data1 data2))
      (setq data-length 2
	    bytes (list status data1)))
    (cffi:with-foreign-objects ((pkt-buffer :char 1024)
				(data :unsigned-char data-length))
      (loop for byte in (mapcar #'floor bytes)
	    for i from 0
	    do (setf (cffi:mem-aref data :unsigned-char i) byte))
      (let ((pkt (packet-list-init pkt-buffer)))
	(packet-list-add pkt-buffer 1024 pkt timestamp data-length data)
	(send (getf *midi-client* :out-port) destination pkt-buffer)))))

(defun midi-send-at (hosttime destination status channel data1 &optional data2)
  (send-midi-message destination hosttime
		     (+ (1- (alexandria:clamp channel 1 16)) status)
		     data1 data2))

(defun midi-send (destination status channel data1 &optional data2)
  (apply #'midi-send-at
    0 destination status channel data1 (when data2 (list data2))))

;; #### NOTE: is this needed? Doesn't MIDIClientDispose perform this cleanup?
(defun dispose-resources-of-client (client)
  "Disposes resources of given client."
  (let ((in-port (getf client :in-port)))
    (dolist (src (getf client :sources))
      (port-disconnect-source in-port src))
    (port-dispose in-port))
  (port-dispose (getf client :out-port))
  (dolist (end-pnt (getf client :virtual-endpoints))
    (endpoint-dispose end-pnt))
  (client-dispose (getf client :client)))


;; ==========================================================================
;; MIDI Notifications
;; ==========================================================================

(defconstant +setup-changed+ 1)
(defconstant +object-added+ 2)
(defconstant +object-removed+ 3)
(defconstant +property-changed+ 4)
(defconstant +thru-connections-changed+ 5)
(defconstant +serial-port-owner-changed+ 6)
(defconstant +io-error+ 7)

(defvar *midi-notify-handler* nil)

(defun add-midi-notify-callback (handle)
  (alexandria:appendf *midi-notify-handler* (list handle)))

(defun set-midi-notify-callback (handle)
  (setf *midi-notify-handler* (list handle)))


(cffi:defcallback handle-notification
    :void ((message :pointer) (ref-con :pointer))
  (declare (ignorable message ref-con))
  (cffi:with-foreign-slots
      ((message-id message-size) message (:struct notification))
    (handler-case
	(dolist (h *midi-notify-handler*)
	  (funcall h message-id message-size))
      (error (c) (format t "~a error...while call handle-notification~%" c)))))

(defun initialize ()
  "Prepare a midi-client and required resources."
  (unless *midi-client*
    (cffi:with-foreign-objects ((client 'client-ref)
				(in-port 'port-ref)
				(out-port 'port-ref))
      (with-cf-strings ((client-name "CommonLispClient")
			(in-port-name "CommonLispClientInputPort")
			(out-port-name "CommonLispClientOutputPort"))
	(client-create client-name
		       (cffi:callback handle-notification)
		       (cffi-sys:null-pointer)
		       client)
	(let ((client (cffi:mem-ref client 'client-ref)))
	  (input-port-create client in-port-name
			     #+ccl(cffi:callback handle-packets)
			     #-ccl(cffi:foreign-symbol-pointer "handlePackets")
			     (cffi-sys:null-pointer)
			     in-port)
	  (output-port-create client out-port-name out-port)
	  #-ccl (create-packets-handler-thread)
	  (setf *midi-client*
		(list :client client
		      :in-port (cffi:mem-ref in-port 'port-ref)
		      :out-port (cffi:mem-ref out-port 'port-ref)
		      :sources nil
		      :handlers nil
		      :virtual-endpoints nil)))))))
