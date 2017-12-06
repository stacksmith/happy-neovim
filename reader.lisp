(in-package #:happy-neovim)
;;==============================================================================
;;
;; This is the lowest-level decoder.  This layer assembles multibyte atoms,
;; decodes headers, and reports kind and value information
;;
;; Instead of using the highly overloaded word 'type', %KIND is used to
;; enumerate messagepack types.  The decoder provides unique %KIND values
;; on output, but each %KIND enumeration uses a unique bit.  Parsers use this
;; feature to combine allowed %KINDs into a bitmask for easy typechecking.
;;
;; Several synthesized (non-messagepack) %KINDS are provided:
;; - %C is a character representing elements of a %STR aggregate;
;; - %BYTE is a byte representing elements of a %BIN and %EXT aggregates;
;; - %PAIR represents elements of %MAP type and always has 2 values.
;;
;; OBJ-HANDLER is invoked with %KIND, VALUE and AGGREGATEP parameters.
;; AGGREGATEP means that the KIND is a header of a multivalue type such as
;; an array or a string, and VALUE is the count of elements to expect.  Note
;; that that the count may be 0!
;;
;; Reader keeps track of a global byte count (total), and aggregate-local down
;; counter (dcnt) and up counter (ucnt).  These are reentrant and nested when
;; aggregate elements are also aggregates.  When dcnt decrements to 0, all
;; elements of an aggregate had been processed, and a previous state is
;; restored.
;;
;; READER-PUSH and READER-POP methods are invoked to save and restore state.
;; Derived classes may hook these methods - READER-PUSH :after
;; READER-POP :before to store private data or perform additional actions on
;; entry and exit of aggregates.
;;
;;
;;==============================================================================
;; NOT YET IMPLEMENTED:
;;
;; floats 
;;
(eval-when  (:compile-toplevel :load-toplevel :execute)
  (defparameter %ERR       #x00000)
  (defparameter %BOOL      #x00100)
  (defparameter %U         #x00200)
  (defparameter %S         #x00400)
  (defparameter %BYTE      #x00800)
  (defparameter %C         #x01000)
  (defparameter %SINGLE    #x02000)
  (defparameter %DOUBLE    #x04000)
  ;; aggreagete types
  (defparameter %STR       #x08000) 
  (defparameter %ARR       #x10000)
  (defparameter %MAP       #x20000)
  (defparameter %EXT       #x40000)
  (defparameter %BIN       #x80000)
  (defparameter %PAIR     #x100000)
  ;; some useful bitmasks for parsing
  (defparameter %*         #xFFF00)
  (defparameter %ANY       #xFFF00)
  (defparameter %NONE      0)  
  (defparameter %AGG    (+ %ARR %MAP %STR %BIN %EXT))
  (defparameter %I      (+ %U %S %BYTE))
  (defparameter %FLOAT  (+ %SINGLE %DOUBLE))
  (defparameter %N      (+ %I %FLOAT)))

;; The array containing decoder information (see end of this file)
(defparameter *decode* nil)

;;==============================================================================
;; kinds are almost usable in hex; this provides a human-readable output.
(defun dump-kind (kind &optional (s t))
  (if kind
      (mapc (lambda (try)
	      (unless (zerop  (logand (symbol-value try) kind))
		(format s "~A " try)))
	    '(%BOOL %U %S %BYTE %C %SINGLE %DOUBLE
	      %STR %ARR %MAP %EXT %BIN %PAIR))
      (format s "-NIL-"))
  (values))
;;==============================================================================
;;==============================================================================
;;==============================================================================
(defclass reader ()
  ((obj-handler  :accessor obj-handler  :initform  #'null-handler)
   ;;private data!
   (byte-handler :accessor byte-handler :initform #'$HEADER);;
   (val      :accessor val   :initform nil);; aggregated value; not always used!
   (total    :accessor total :initform 0)
   (bytes    :accessor bytes :initform 0) ;; counter for chars and bin bytes.
   (post-acc :accessor post-acc :initform nil);;call upon accumulation done...
   ;; STACK and count aggregates
   (el-handler :accessor el-handler :initform #'$HEADER)
   (dcnt     :accessor dcnt  :initform most-positive-fixnum)
   (ucnt     :accessor dcnt  :initform 0)
   (stack    :accessor stack :initform (make-array 100 :fill-pointer 0))
   (level    :accessor level :initform 0)
))
;;==============================================================================
;; Handle stacking for aggregate types...
(defgeneric reader-push (reader element-count element-handler))
(defgeneric reader-pop  (reader))

(defmethod reader-push ((reader reader) element-count element-handler)
  (with-slots (el-handler dcnt ucnt stack level) reader 
    (vector-push ucnt stack);; always first
    (vector-push el-handler stack)
    (vector-push dcnt stack)
    (setf el-handler element-handler ;; for all elements
	  dcnt element-count
	  ucnt 0)
    (incf level)))

(defmethod reader-pop ((reader reader))
  (with-slots (el-handler dcnt ucnt stack level) reader
    (setf dcnt       (vector-pop stack)
	  el-handler (vector-pop stack)
	  ucnt       (vector-pop stack))
    (decf level)))

(defun reader-multipop (reader)
  (with-slots (ucnt dcnt byte-handler el-handler) reader
    (loop while (zerop (progn
			 (incf ucnt)
			 (decf dcnt))) do
	 (reader-pop reader))))


;;------------------------------------------------------------------------------
(defun print-reader (o &optional (s t))
  (format s " #~A (~A)"
	  (level o)
	  (byte-handler o)))
(defmethod print-object ((o reader) s)
  (print-unreadable-object (o s :type t)
    (print-reader o s)))

;;==============================================================================
;; default object handler 
(defun null-handler (reader kind value aggregatep)
  (with-slots (ucnt dcnt stack)reader
    (format t "NULL-HANDLER ~A ~A ~A " kind value aggregatep)
    (dump-kind kind)
    (terpri)
)  nil)

;;------------------------------------------------------------------------------
(defun decode (reader value)
  (incf (total reader))
  (funcall (byte-handler reader) reader value)
  reader)

;;==============================================================================
;; OUTPUT

;;------------------------------------------------------------------------------
;; Simple output for non-aggregates
(defun out-simple (reader kind value)
  (with-slots (byte-handler el-handler obj-handler ) reader
    (funcall obj-handler reader kind value nil)
    (reader-multipop reader)
    (setf byte-handler el-handler)))

;;------------------------------------------------------------------------------
;; output an aggregate header.
(defun out-agg (reader kind size eldec)
  (with-slots (byte-handler el-handler obj-handler) reader
    (funcall obj-handler reader kind size t)
    (if (zerop size)
	(reader-multipop reader)
	(reader-push reader size eldec))
    (setf byte-handler el-handler);; inside or at same level...
  size))


;;==============================================================================
;; A common mode of operation is to accumulate unsigned values - useful for
;; -multibyte integers;
;; -counts
;; To facilitate that, we use:
;; -bytes to keep remaining byte count for accumulation;
;; -val as an accumulator
;; -post-accum vector called on final accumulation...
;;
;; ACC handler will accumulate value and report when done.
;; Note that the post-acc function has the same signature as normal handlers
;; for extreme convenience!
(defun $ACC (reader value )
  (with-slots (val bytes post-acc) reader
    (setf val (+ value (ash val 8)))
    (when (zerop (decf bytes))
      (setf (byte-handler reader) post-acc)
      (funcall post-acc reader val))))

(defun $SACC (reader value)
  (setf (byte-handler reader) #'$ACC)
  ($ACC reader (signed-value value)))

;; installer for an accumulator...
(defun accumulate (reader count acc action)
  (with-slots (val bytes byte-handler post-acc) reader
    (setf val 0
	  bytes count
	  byte-handler acc
	  post-acc action)))
;;==============================================================================
;; Header calls the handler immediately after sorting out how to do that.
;;
(defun $HEADER (reader byte)
  (with-slots (byte-handler val stack) reader
    (let ((code (aref *decode* byte)))
      (funcall (setf byte-handler (car code))
	       reader (cdr code)))))
;;==============================================================================
;; UNSIGNED
;;
(defun $U0 (reader value)
  (out-simple reader %U value))

(defun $U (reader bytes)
  (accumulate reader bytes #'$ACC #'$U0 )) 

;;==============================================================================
;;==============================================================================
;; SIGNED values: first byte must be converted; rest just work out.
(defun signed-value (byte)
  (if (logbitp 7 byte )  (- byte #x100 )  byte ))

;; S0 called with a value must convert it...
(defun $S0 (reader value)
  (out-simple reader %S (signed-value value)))

;; Use a signed accumulator for first value (it installs unsigned one).
;; 
(defun $S (reader bytes)
  (accumulate reader bytes #'$SACC
	      (lambda (reader value) (out-simple reader %S value))))

;; Pair is a fake kind used to aggregate pairs of values for MAP.
(defun $PAIR (reader value)
  (out-agg reader %PAIR 2 #'$HEADER)
  ($HEADER reader value))

;;==============================================================================
;; MAP
(defun $MAP0 (reader value)
  (out-agg reader %MAP value #'$PAIR))

(defun $MAP (reader bytes)
  (accumulate reader bytes #'$ACC #'$MAP0))

;;==============================================================================
;; ARR
(defun $ARR0 (reader size)
  (out-agg reader %ARR size #'$HEADER))

(defun $ARR (reader bytes)
  (accumulate reader bytes #'$ACC #'$ARR0))


;;==============================================================================
;; STR
(defun $STR0 (reader cntbytes)
  (out-agg reader %STR cntbytes #'$CHAR))

(defun $STR (reader cntbytes)
  (accumulate reader cntbytes #'$ACC #'$STR0))

;;==============================================================================
(defun $CHAR (reader value)
  (out-simple reader %C value))

;;==============================================================================
;; ERR
(defun $ERR0 (reader value)
  (setf (byte-handler reader) #'$HEADER ))
;;==============================================================================
;; BOOL
(defun $BOOL0 (reader value)
  (out-simple reader %BOOL value))

;;==============================================================================
;; BIN
(defun $BIN0 (reader value)
  (out-agg reader %BIN value #'$BYTE))

(defun $BIN (reader cntbytes)
  (accumulate reader cntbytes #'$ACC #'$BIN0))

(defun $BYTE (reader value)
  (out-simple reader %BYTE value))
;;==============================================================================
;; EXT NOTE: passing type as the first; count is +1.  Even a zero-length ext
;; will be followed by type
(defun $EXT0 (reader size)
  (out-agg reader %EXT (1+ size) #'$EXT-TYPE))

(defun $EXT (reader sizebytes)
  (accumulate reader sizebytes #'$ACC #'$EXT0))

;; Just to report a signed type prior to bytes.
(defun $EXT-TYPE (reader value)
  (setf (byte-handler reader) #'$BYTE)
  (out-simple reader %S (signed-value value)))

;;==============================================================================
;; FLOAT
(defun $SINGLE (reader bytecnt)
  (accumulate reader bytecnt #'$ACC
	      (lambda (reader value) (out-simple reader %SINGLE value))))

(defun $DOUBLE (reader bytecnt)
  (accumulate reader bytecnt #'$ACC
	      (lambda (reader value) (out-simple reader %DOUBLE value))))


(defun init ()
  (setf *decode*
	(make-array
	 256
	 :initial-contents
	 (mapcar (lambda (q) (cons (cadar q) (cdr q)))
		 '((#'$U0 . 0) (#'$U0 . 1) (#'$U0 . 2) (#'$U0 . 3) (#'$U0 . 4)
		   (#'$U0 . 5) (#'$U0 . 6) (#'$U0 . 7) (#'$U0 . 8) (#'$U0 . 9)
		   (#'$U0 . 10) (#'$U0 . 11) (#'$U0 . 12) (#'$U0 . 13) (#'$U0 . 14)
		   (#'$U0 . 15) (#'$U0 . 16) (#'$U0 . 17) (#'$U0 . 18) (#'$U0 . 19)
		   (#'$U0 . 20) (#'$U0 . 21) (#'$U0 . 22) (#'$U0 . 23) (#'$U0 . 24)
		   (#'$U0 . 25) (#'$U0 . 26) (#'$U0 . 27) (#'$U0 . 28) (#'$U0 . 29)
		   (#'$U0 . 30) (#'$U0 . 31) (#'$U0 . 32) (#'$U0 . 33) (#'$U0 . 34)
		   (#'$U0 . 35) (#'$U0 . 36) (#'$U0 . 37) (#'$U0 . 38) (#'$U0 . 39)
		   (#'$U0 . 40) (#'$U0 . 41) (#'$U0 . 42) (#'$U0 . 43) (#'$U0 . 44)
		   (#'$U0 . 45) (#'$U0 . 46) (#'$U0 . 47) (#'$U0 . 48) (#'$U0 . 49)
		   (#'$U0 . 50) (#'$U0 . 51) (#'$U0 . 52) (#'$U0 . 53) (#'$U0 . 54)
		   (#'$U0 . 55) (#'$U0 . 56) (#'$U0 . 57) (#'$U0 . 58) (#'$U0 . 59)
		   (#'$U0 . 60) (#'$U0 . 61) (#'$U0 . 62) (#'$U0 . 63) (#'$U0 . 64)
		   (#'$U0 . 65) (#'$U0 . 66) (#'$U0 . 67) (#'$U0 . 68) (#'$U0 . 69)
		   (#'$U0 . 70) (#'$U0 . 71) (#'$U0 . 72) (#'$U0 . 73) (#'$U0 . 74)
		   (#'$U0 . 75) (#'$U0 . 76) (#'$U0 . 77) (#'$U0 . 78) (#'$U0 . 79)
		   (#'$U0 . 80) (#'$U0 . 81) (#'$U0 . 82) (#'$U0 . 83) (#'$U0 . 84)
		   (#'$U0 . 85) (#'$U0 . 86) (#'$U0 . 87) (#'$U0 . 88) (#'$U0 . 89)
		   (#'$U0 . 90) (#'$U0 . 91) (#'$U0 . 92) (#'$U0 . 93) (#'$U0 . 94)
		   (#'$U0 . 95) (#'$U0 . 96) (#'$U0 . 97) (#'$U0 . 98) (#'$U0 . 99)
		   (#'$U0 . 100) (#'$U0 . 101) (#'$U0 . 102) (#'$U0 . 103) (#'$U0 . 104)
		   (#'$U0 . 105) (#'$U0 . 106) (#'$U0 . 107) (#'$U0 . 108) (#'$U0 . 109)
		   (#'$U0 . 110) (#'$U0 . 111) (#'$U0 . 112) (#'$U0 . 113) (#'$U0 . 114)
		   (#'$U0 . 115) (#'$U0 . 116) (#'$U0 . 117) (#'$U0 . 118) (#'$U0 . 119)
		   (#'$U0 . 120) (#'$U0 . 121) (#'$U0 . 122) (#'$U0 . 123) (#'$U0 . 124)
		   (#'$U0 . 125) (#'$U0 . 126) (#'$U0 . 127)
		   
		   ;;80-8F
		   (#'$MAP0 . 0) (#'$MAP0 . 1) (#'$MAP0 . 2) (#'$MAP0 . 3)
		   (#'$MAP0 . 4) (#'$MAP0 . 5) (#'$MAP0 . 6) (#'$MAP0 . 7)
		   (#'$MAP0 . 8) (#'$MAP0 . 9) (#'$MAP0 . 10) (#'$MAP0 . 11)
		   (#'$MAP0 . 12) (#'$MAP0 . 13) (#'$MAP0 . 14)(#'$MAP0 . 15) 
		   ;;90-9F
		   (#'$ARR0 . 0) (#'$ARR0 . 1) (#'$ARR0 . 2) (#'$ARR0 . 3)
		   (#'$ARR0 . 4) (#'$ARR0 . 5) (#'$ARR0 . 6) (#'$ARR0 . 7)
		   (#'$ARR0 . 8) (#'$ARR0 . 9) (#'$ARR0 . 10) (#'$ARR0 . 11)
		   (#'$ARR0 . 12) (#'$ARR0 . 13) (#'$ARR0 . 14)(#'$ARR0 . 15)
		   
		   ;;A0-BF
		   (#'$STR0 . 0 ) (#'$STR0 . 1 )     (#'$STR0 . 2 ) (#'$STR0 . 3 )
		   (#'$STR0 . 4 ) (#'$STR0 . 5 )     (#'$STR0 . 6 ) (#'$STR0 . 7 )
		   (#'$STR0 . 8 ) (#'$STR0 . 9 )     (#'$STR0 . 10 ) (#'$STR0 . 11 )
		   (#'$STR0 . 12 ) (#'$STR0 . 13 )   (#'$STR0 . 14 ) (#'$STR0 . 15 )
		   (#'$STR0 . 16 ) (#'$STR0 . 17 )   (#'$STR0 . 18 ) (#'$STR0 . 19 )
		   (#'$STR0 . 20 ) (#'$STR0 . 21 )   (#'$STR0 . 22 ) (#'$STR0 . 23 )
		   (#'$STR0 . 24 ) (#'$STR0 . 25 )   (#'$STR0 . 26 ) (#'$STR0 . 27 )
		   (#'$STR0 . 28 ) (#'$STR0 . 29 )   (#'$STR0 . 30 ) (#'$STR0 . 31 )
		   ;;C0
		   (#'$BOOL0 . nil)
		   ;;C1	
		   (#'$ERR0 . 0)
		   
		   ;;C2
		   (#'$BOOL0 . nil) (#'$BOOL0 . t)
		   ;;C4
		   (#'$BIN . 1)(#'$BIN . 2)(#'$BIN . 4)
		   ;;C7
		   (#'$EXT . 1)(#'$EXT . 2)(#'$EXT . 4)
		   ;;CA
		   (#'$SINGLE4 . 4)(#'$DOUBLE8 . 8)
		   ;;CC
		   (#'$U . 1)     (#'$U . 2)     (#'$U . 4)     (#'$U . 8)
		   ;;D0
		   (#'$S . 1)     (#'$S . 2)     (#'$S . 4)     (#'$S . 8)
		   ;;D4
		   (#'$EXT0 . 1) (#'$EXT0 . 2) (#'$EXT0 . 4) (#'$EXT0 . 8) (#'$EXT0 . 16)
		   ;;D9
		   (#'$STR . 1)  (#'$STR . 2)  (#'$STR . 4)
		   ;;DC
		   (#'$ARR . 2) (#'$ARR . 4)
		   ;;DE
		   (#'$MAP . 2) (#'$MAP . 4)
		   ;;E0
		   (#'$S0 . #xE0)(#'$S0 . #xE1) (#'$S0 . #xE2)(#'$S0 . #xE3)
		   (#'$S0 . #xE4)(#'$S0 . #xE5) (#'$S0 . #xE6)(#'$S0 . #xE7)
		   (#'$S0 . #xE8)(#'$S0 . #xE9) (#'$S0 . #xEA)(#'$S0 . #xEB)
		   (#'$S0 . #xEC)(#'$S0 . #xED) (#'$S0 . #xEE)(#'$S0 . #xEF)
		   (#'$S0 . #xF0)(#'$S0 . #xF1) (#'$S0 . #xF2)(#'$S0 . #xF3)
		   (#'$S0 . #xF4)(#'$S0 . #xF5) (#'$S0 . #xF6)(#'$S0 . #xF7)
		   (#'$S0 . #xF8)(#'$S0 . #xF9) (#'$S0 . #xFA)(#'$S0 . #xFB)
		   (#'$S0 . #xFC)(#'$S0 . #xFD) (#'$S0 . #xFE)(#'$S0 . #xFF))))))
(init)
