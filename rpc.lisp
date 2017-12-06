(in-package :hn)
;;=============================================================================
;; Layer 3 - lockstep parser - runtime!
;;
;; This layer implements a complete parsing engine for arbitrary messagepack
;; sequences.  Parsing is acomplished by using a DSL of sorts for describing
;; expected messagepack data tree,  which the parser locks onto.  Data comes
;; in as kind/value pairs.
;;
;; Generally, matching is specified by a single value to match, or a list
;; of alternatives.  Kinds may be matched against masks allowing more than
;; one kind, while values must be either * for any value, or an exact value.
;;
;; An extra compile-time translation layer is implemented in plan.lisp to
;; simplify the construction of plans, (string tree generation etc).
;;
;; The syntax is very simple.  A plan is a list that starts with a KIND
;; position, followed by a value slot, followed by a kind slot, etc.  Every
;; position may be an atom or a list of alternatives.  If alternatives are
;; used, they must all start with an appropriate kind or value, and each must
;; continue to the bitter end (for exceptions see below).
;;
;; Basic types declared in reader are used to fill KIND positions.  Example:
;;
;; '(%U * %ARR 3 %U 4... reads any unsigned value followed by an array of 3
;;                       elements, the first one must be a 4.
;;
;; '(%ARR (3 %U 1 ...    reads an array of 3 or 4 elements, 1 being the 
;;        (4 %S -3 ..    first element in the 3 case, or -3 in the 4-case.
;;
;; '(%STR 4 #\a #\b #\c  reads a string of 4 characters, with the first 3
;;                       as shown.  Note that for strings and BINs, values
;;                       are listed without types (as character is implied).
;;
;; In reality, numeric values that stand for kinds and characters must be used.
;; Since quoted lists are not evaluated, you must enter kinds as reader-
;; evaluated objects, i.e. #.%U and #.%ARR - or use the convenience parser in
;; plan.lisp,
;;
;; Characters must be converted to their UTF8 byte equivalents and matched
;; a byte at a time.  plan.lisp takes care of that.
;;
;; In addition, certain keyword parameters may be used to indicate side
;; effects, such as capturing values, calling functions, and control transfer.
;; Generally side effects must follow value slots and contain a single value
;; (which can be a list in case of :FUN) :LOOP is an exception as it has no
;; values.
;;
;; - :VAR i  causes the value to be captured into a variable indexed by i.
;; - :GOTO   symbol transfers control to a plan stored in a global symbol.
;; - :STR i  placed after a %STR * value conses up the entire string and
;;           stores it in a variable indexed by i
;; - :FUN (name ...) Invokes a global function by the name with parameters.
;;           parameters may be one of
;;           - integer indices, requesting current values of collected variables
;;           - anything else, which will be passed to the function.
;; Note: index 0 is used as a counter for aggregate values (in some cases).
;;
;; - :LOOP   placed after an aggregate object declaration, causes the :LOOP
;;           location to be stored for future use.  The rest of the plan is
;;           followed as usual.  At runtime, should the plan run off the end
;;           of its list, the :LOOP position is forced into the parser -
;;           as long as the corresponding object down-counter is positive.
;;
;;           If the counter happens to run out, the loop terminates.  Since the
;;           aggregate object associated with the loop is done, it goes out
;;           of scope, and control is transfered to the previously declared
;;           loop, in a higher scope.
;;
;;           Note that scoping is dynamic, and once a :LOOP is declared,
;;           the loop will be unwound at the end of the list (hopefully,
;;           matching the end of the current messagepack aggregate!).  Even
;;           a :GOTO transfer to another location will be returned to the loop
;;           construct!
;;
;;------------------------------------------------------------------------------
;; Runtime lockstep tracking of incoming data against plans.
;; There is no nesting; tags are used to create loops.
(defclass rpc (support)
  ((param :accessor param :initform (make-array 64))
   (plan    :accessor plan    :initform nil :initarg :plan)
   ;; we assemble UTF8 characters here.
   (utf8-mode :accessor utf8-mode :initform nil) ;when t, do UTF8
   (utf8-rem :accessor utf8-rem :initform 0);remaining UTF8 bytes
   (utf8-acc :accessor utf8-acc :initform 0);accumulated code
   ;; string-builder accumulates strings here:
   (str      :accessor str :initform nil)
   ;; The handler may be switched for more specific work, to wit: strings.
;;   (str-vec :accessor str-vec :initform nil)
;;   (str-char :accessor str-char :initform 0)
;;   (str-)
;;   (ch  :accessor ch  :initform 0);; UTF8 character accumulator
   ))

(defmethod initialize-instance :after ((rpc rpc)&key)
  (setf (obj-handler rpc) #'oh-plan))

;;------------------------------------------------------------------------------
(defmethod print-object ((o treader) s)
  (print-unreadable-object (o s :type t)
    (print-treader o s)
    (format s "~&plan:~A"  (plan o))))
;;------------------------------------------------------------------------------
;;=============================================================================
;; Do not call directly! Called via oh vector by treader...
;;
(defun oh-plan (parser kind value agg)
    ;;(format t "on-data ~A ~A ~&" kind value)
  ;; (format t "dcnt ~A count: ~A~&" (dcnt parser) (cnt parser))
  (with-slots (plan mark utf8-mode) parser
    (unless plan
 ;;     (format t "~&PLAN ran on ~X ~X~&" kind value )
      (setf plan mark))
    (case kind
      (#.%C       
       (if utf8-mode
	   (if (< value 128)	  
	       (on-val parser value)
	       (utf8-first parser value))
	   (on-val parser value)))
      (#.%str
       (setf utf8-mode nil)
       (on-kind parser kind) 
       (on-val parser  value))
      (t
       (progn
	 (on-kind parser kind) 
	 (on-val parser  value))))))

;; Process first byte of a utf8 character
(defun utf8-first (parser value)
  (with-slots (obj-handler utf8-rem utf8-acc) parser
    (multiple-value-setq (utf8-rem utf8-acc)
      (cond
	((< value 128) (values 0 value) )
	((= #b110   (ldb (byte 3 5) value)) (values 1 (logand #x1F value)))
	((= #b1110  (ldb (byte 4 4) value)) (values 2 (logand #x0F value)))
	((= #b11110 (ldb (byte 5 3) value)) (values 3 (logand #x07 value)))
	(t (error "Unexpected first UTF8 byte ~X" value))))
    (setf obj-handler #'oh-utf8)))

(defun oh-utf8 (parser kind value agg)
  (with-slots (obj-handler utf8-rem utf8-acc) parser
    (if (and (= %C kind)
	     (= #b10000000 (logand #xC0 value)))
	(progn
	  (setf utf8-acc (+ (ash utf8-acc 6) (logand #x3F value)))
;;	  (format t "UTF8 byte ~A; UTF8-ACC NOW ~A~&" value utf8-acc)
	  (if (zerop (decf utf8-rem))
	      (progn
		(setf obj-handler #'oh-plan)
		(on-val parser utf8-acc))))
	(progn
	  (setf obj-handler #'oh-plan)
	  (error "Unexpected data ~X ~X in UTF8 sequence" kind value)))))
;;=============================================================================
;; RUN-TIME INTERPRETER
;;
;;------------------------------------------------------------------------------
;; ON-KIND
;;
;; Kinds are numbers or alternatives.  No side effects are allowed here.
(defun on-kind (parser kind)
;;  (format t "on-kind ~A~&" kind)
  (with-slots (plan stack param) parser
    (setf
     plan
     (let ((car-plan (car plan)))
       (typecase car-plan
	 ;;---------------------------------------------------------------------
	 (list
	  (cdr (assoc kind plan :test (lambda (x y)(plusp (logand x y))))))
	 ;;---------------------------------------------------------------------
	 (number
	  (if (plusp (logand car-plan kind))
	      (cdr plan)
	      (error "Unmatched kind ~A in ~A" kind plan)))
	 ;;---------------------------------------------------------------------

	 (symbol
	  (case car-plan
	    (:LOOP
	     (setf plan (set-mark parser (cdr plan)))
	     (on-kind parser kind));; and process immediately!
	    (:INDEX
	     #||
	     (format t ":~A:INDEX param ~A set to ~A~&"
		     (level parser)
		     (cadr plan) (mark-ucnt parser))
||#
;;	     (format t "WELL, kind is ~A; level is ~A~&" kind (level parser))
	     
	     (setf (aref param (cadr plan)) (mark-ucnt parser)
		   plan  (cddr plan))
	     (on-kind parser kind))
	    
	    #||
	    (progn ;; shove the loop location into the one-above-ours
	    (setf (aref (stack parser) (1- (fill-pointer (stack parser))))
	    (setf plan (cdr plan)));; also, shove the counter into
	    
	    (aref (stack parser) (- (fill-pointer (stack parser)) 5)))
	    
	    ||#
	    (t  (error "Unexpected symbol ~A against ~A in ~A"
		       kind car-plan plan))))
	 ;;---------------------------------------------------------------------
	 (t (error "Unexpected thing ~A against ~A in ~A"
		   kind car-plan plan)))))))
;;------------------------------------------------------------------------------
;; ON-VAL
;;
;; %STR * :VAR$ 1 * VAR$ 1 First one, responding to %U length, creates a string;
;; the rest continue to store to it until count runs out.
(defun on-val (parser val)
  ;;---------------------------------------------------------------------------
  ;; Process actual value position
;;  (format t "on-val ~A~&" val)

  (with-slots (param plan utf8-mode) parser
    (let ((car-plan (car plan)))
      (setf plan
	    (cdr ;; in every case we advance to next element
	     (typecase car-plan
	       ;;---------------------------------------------------------------
	       ;; List - handle alternative values...
	       (list (assoc val plan :test (lambda (x y)
					     (or (eq y '*) (equal x y)))))
	       ;;---------------------------------------------------------------
	       ;; Symbol - * is the only usable one
	       (symbol 
		(if (eq car-plan '*)
		    plan
		    (error "Unexpected symbol ~A against ~A in ~A"
			   val car-plan plan)))
	       ;;---------------------------------------------------------------
	       ;; Otherwise, try for an exact match.
	       (t (if (equal car-plan val)
		      plan
		      (error "Mismatched value ~A against ~A in ~A"
			     val car-plan plan))))))
      ;;-----------------------------------------------------------------------
      ;; Now one or more side effects...
      (let (done); some side effects must terminate the value position...
	(loop for car-plan = (car plan)
	   until done
	   while (keywordp car-plan) do;; all start with a keyword
	     (let ((parm (cadr plan)));; convenient 'value' of side effect
	       (setf plan
		     (case car-plan
		       (:VAR ;;(format t "VAR ~A = ~A ~&" parm val)
			     (setf (aref param parm) val);; store data at i
			     (cddr plan))
		      
		       ;; Function is executes while still inside the VALUE
		       ;; slot processing...
		       (:FUN  (data-fun parser parm);; invoke funcaller
			      (cddr plan))
		       (:LOOP (setf done t);;stop here and reparse :LOOP
			      plan)        ;; as a KIND...
		       (:GOTO (symbol-value parm)) ;; simple
		       (:UTF8 (setf utf8-mode t)
			      (cdr plan))
		       (:STR (install-string-builder parser val parm)
			     (setf utf8-mode t)
			     (cddr plan)) ;; shows off a custom handler

		       (t (error "Unexpected keyword ~A" car-plan))))))))
    plan))

#||
(defmethod on-data ((parser rpc) kind value)
  ;;  (format t "on-data ~A ~A ~&" kind value)
  ;; (format t "dcnt ~A count: ~A~&" (dcnt parser) (cnt parser))
  (with-slots (plan) parser
    (if (eq %C kind) ;; character values are sent without kind notification...
	(on-val parser (code-char value))
	(progn
	  (on-kind parser kind) 
	  (on-val parser  value)))))
||#


;; TODO: this needs improvement...
(defun data-fun (parser parm)
  (with-slots (param) parser
;;    (format t "~& param[0] = ~A================ FUN ~A~&" (aref param 0) (car parm))
;    (format t "Level ~A; marklevel ~A~&" (fill-pointer (stack parser)) (marksp parser)) 
    (apply (symbol-function (car parm))
	   parser
	   (mapcar (lambda (p)
		     (typecase p
		       ;;-------------------------------------------------------
		       (number ;;means index into the parameter array
		;;	(format t "AREFING ~A ~&" p)
			(aref param p))
		       (t p)))
		   (cdr parm)))))
;; stringbuilder
(defun install-string-builder (parser length index)
  (with-slots (str obj-handler) parser
    (if (< length 1024)
	(setf str (make-array length :fill-pointer 0 :element-type 'character)
	      (aref (param parser) index) str
	      obj-handler #'string-builder)
	(error "~A is too long for a string" length))))

(defun string-builder (parser kind value agg)
  (with-slots (str obj-handler) parser
    (if (= kind #.%C)
	(vector-push (code-char value) str)
	(error "Char type expected, not ~A ~A" kind value))
    (when (= (fill-pointer str) (array-total-size str))
   ;;   (format t "STRING: ~A~&" str)
      (setf obj-handler #'oh-plan))))


#||
(defun UTF8-1 (parser kind value)
  (with-slots (ch object-handler) parser
    (multiple-value-setq (ch object-handler)
      (cond
	((logbitp 7 value) (values value #'UTF8-1))
	((= #b10 (ldb (byte 2 5) value))
	 (logand value #x1F)))))
  )

(setf *v* (make-instance 'rpc))
(setf (plan *v*) *plan*)
||#
