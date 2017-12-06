(in-package #:hn)
;;==============================================================================
;; Layer 2 - Parser support layer.
;;
;; This layer provides support for keeping a 'mark' associating user data to
;; a specific state.  As the reader pushes the state, the mark is also pushed,
;; and the mark's position on the stack is tracked for access to that state.
;;

#||
;    (format t "NULL-HANDLER [#~A: ~A ~A]: ~A ~A~&"   (fill-pointer stack) dcnt ucnt kind value)


||#
(defclass support (reader)
  ((mark    :accessor mark    :initform nil)
   (marksp  :accessor marksp  :initform 0)
))

(defmethod initialize-instance :after ((r support) &key)
)

(defun print-support (o &optional  (s t))
  (with-slots (dcnt ucnt mark stack) o
    (print-reader o s)
    (format s "marksp ~A" (marksp o))))
(defmethod print-object ((o support) s)
  (print-unreadable-object (o s :type t)
    (print-support o s))
)

;;==============================================================================
;;
;; STACK
;;
;; We shall store some extra items on the stack.
(defmethod reader-push :after ((reader support) ecount ehand)
  (with-slots (dcnt ucnt marksp mark stack) reader
    (vector-push marksp stack)
    (vector-push mark   stack)
;;   (format t "--#~A:PUSHED [~A] ~&" (level reader) ucnt)
    ))


(defmethod reader-pop :before ((reader support))
  (with-slots (dcnt ucnt marksp mark stack) reader
;;    (format t "--#~A:POPPING [~A]~&" (level reader) (aref stack (fill-pointer stack)))
    (setf mark        (vector-pop stack)
	  marksp      (vector-pop stack))))


;;==============================================================================
;; mark
;;
;; Set the mark for this level!  Set ucnt to 0 Return value mark is set to.
(defun set-mark (reader markval)
  (with-slots (mark marksp ucnt stack) reader
    #||
    (format t "SET-MARK !!! we are at level ~A (sp ~A); value: ~A~&"
	    (level reader)
	    marksp
	    (aref stack marksp))
||#
    (setf marksp (fill-pointer stack)
;;	  ucnt 0;; reset count for loop to 0.
;;	  (aref stack marksp) 7
	  mark markval)))

;;==============================================================================
;; mark-ucnt - retreive ucnt for mark in effect.
;; used to get counter for looping structures.
(defun mark-ucnt (reader)
  (with-slots (ucnt stack marksp) reader
    (if (= marksp (fill-pointer stack))
	ucnt
	(aref stack marksp))    ))
