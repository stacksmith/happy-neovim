(in-package :hn)
;;============================================================================
;; Plan parsing and compile-time conveniences
;;
;; While it is entirely possible to write a plan directly, the utility
;; compile-time plan builder provides some niceties and error checking.
;;
;; The syntax is (parse-plan '(...)).  The syntax is identical to the
;; lockstep description, with a couple of extra features.
;;
;; -Kinds and combination masks such as %U or %N are converted automatically
;; -An additional :STRINGS keyword declares strings that are automatically
;;  converted to a tree of characters indexed by the string length.  For
;;  example,
;;
;;  :STRINGS ("over" %I *)("ovum" %U 4)("under" %U 3)  ->>>
;;
;;  %STR (5 #\u #\n #\d #\e #\r %U 3)
;;       (4 #\o #\v (#\e #\r %I *)
;;                  (#\u #\m %U 4)
;;
;;  This is certainly more convenient and less error-prone than sorting
;; strings by hand and counting characters.  However the neovim crew managed
;; to screw up the put highlight_set to require manual intervention...
;;
;; Update: UTF8 characters are broken into constituent bytes.  Matching
;; happens on byte-by-byte level, not character-by-character, to be
;; consistent with MessagePack string lengths being byte lengths!

(defparameter *labels* nil)
;;-----------------------------------------------------------------------------
;; at cdr prev, try to find val as a single item or in an alternative list.
;; Create if necessary.  Return list containing val in car.
(defun parse-maybe-make (prev val)
  (let ((plan (cdr prev)))
    (if plan
	(if (listp (car plan))
	    ;; a list:
	    (or (assoc val plan);; either a match or insert
		(car (setf (cdr prev) (cons (list val) plan ))))
	    ;; not a list...
	    (if (equal val (car plan))
		plan ;find or bifurcate
		(car (setf (cdr prev) (list (list val) plan )))))
	(setf (cdr prev)
	      (list val)))))
;; ----------------------------------------------------------------------------
;; Add a string to the plan, at a point of string alternatives...
(defun parse-add-string (prev string data)
  (let ((p prev))
    (setf p (parse-maybe-make p (length string)))
    (loop for char across string
       do (setf p (parse-maybe-make p char)))
    (setf (cdr p) (parse-plan-kind data ))) 
  prev )

;;-----------------------------------------------------------------------------
;; stringalts are in the form ("name" ...)
(defun parse-add-strings (prev alts)
;;  (format t "PREV: ~A ALTS ~A~&"prev alts)
  (loop for alt in alts do
       (parse-add-string prev (car alt) (cdr alt)))
  prev)

(defun parse-plan-val (map)
;;  (format t "VAL: ~A~&" map)
  (when map
    (typecase (car map)
      (symbol
       (if (eq '* (car map))
	   (cons '* (parse-plan-kind  (cdr map))))
       )
      (list
       (prog1 (loop for q in map
		 collect (parse-plan-val q))   ))
      (t  (cons (car map) (parse-plan-kind (cdr map)))))))

(defun parse-plan-kind (map)
  (when map
    (typecase (car map)
      (keyword
       (case (car map)
	 (:STRINGS
	  (let ((q (list %STR)))
	    (append (parse-add-strings q (cdr map)) )))
	 (:LOOP  (cons :LOOP (parse-plan-kind (cdr map))))
	 (:UTF8  (cons :UTF8 (parse-plan-kind (cdr map))))
	 (t;; remaining keywords always take 1 parameter
	  (cons (car map) (cons (cadr map) (parse-plan-kind (cddr map)))))))
      (symbol
       (if (eq '%C (car map))
	   (parse-plan-val (cdr map))
	   (if (eq '* (car map))
	       (cons '* (parse-plan-kind (cdr map)))
	       (cons (symbol-value (car map))
		     (parse-plan-val (cdr map))))))
      ;; scope labels
      (list
       (loop for q in map
	  collect (parse-plan-kind q)))
      (t
       (cons (car map) (parse-plan-val (cdr map)))))
    ))

(defun parse-plan (map)
  (parse-plan-kind map))


(defparameter *q*
  (parse-plan
   '(%U 1
     (%U (2 %S 1)
         (3 %S 2))
     (%S 3)
     )))
#||
(defun foreach-utf8-byte (char fun)
  (let ((c (char-code char)))
    (cond
      ((zerop (ash c -7)) (funcall fun c))
      ((zerop (ash c -11))
       (funcall fun (+ #xC0 (ldb (byte 5 6) c)))
       (funcall fun (+ #x80 (ldb (byte 6 0) c))))
      ((zerop (ash c -16))
       (funcall fun (+ #xE0 (ldb (byte 4 12) c)))
       (funcall fun (+ #x80 ( ldb (byte 6 6) c)))
       (funcall fun (+ #x80 (ldb (byte 6 0) c))))
      ((zerop (ash c -21))
       (funcall fun (+ #xF0 (ldb (byte 3 18) c)))
       (funcall fun (+ #x80 (ldb (byte 6 12) c)))
       (funcall fun (+ #x80 (ldb (byte 6 6) c)))
       (funcall fun (+ #x80 (ldb (byte 6 0) c))))
      (t (error "Illegal UTF8 character ~X" c)))))

||#
