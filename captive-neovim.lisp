(in-package :hn )
;;==============================================================================
;; captive neovim toy
;;
;; This is the raison d'etre of the messagepack interface.  It provides a
;; captive neovim class and the communication mechanism for driving it and
;; receiving screen updates.
;;
;; It is important to consider that the entire RPC mechanism is a state
;; machine, and is driven by feeding it a byte at a time, without any
;; multithreading, or the risk of lockup due to data being unavailable or
;; comm link problems.
;;
;; In addition, the synchronous nature of the project makes it very clear
;; when things happen.  Read a byte from the comm link, feed it to the
;; parser.  After a while some functions are called as the plan is followed.
;; If no data is available - do something else.  Or do something else at
;; any point - the state of the parser is preserved.
;;
;;
;; As most things in life, this is work in progress, and some liberties have
;; been taken to make it work (largely in the enumeration of objects for
;; the mode-info-set plan)...
;;
;;=============================================================================
;; Top of the plan.  Parse an incoming packet.  Note the loop at the top!
;; Also note the :GOTO after we parse the data array
(defparameter *plan*
  (parse-plan
   '(:LOOP
     (%ARR
      ;; RESPONSE: 1 <id> <error> <data>
      (4 %U 1 %U * :VAR 1  %ANY * :VAR 2 %ANY * :VAR 3  :FUN (RESPONSE  1 2 3))
      ;; REDRAW    2 "redraw" <var data array>
      (3 %U 2  :STRINGS ("redraw" %ARR * :GOTO *plan-redraw*))))))

;;-----------------------------------------------------------------------------a-
;; Redraw request is handled here.  Note the :LOOP at the top - processing
;; every component of the variable-size redraw request.
;; Each request is also a variable size array; because of the disastrous
;; implementation of the redraw protocol, "highlight_set" command comes in two
;; flavors - two or three elements total.  Instead of specifying data as
;; a variable size array, as any script kiddie would do, the NeoVIM team
;; decided to let you keep the entire array size, and subtract one for the
;; "highlight_set" string component.  By the time we parse the string,
;; the size is gone, and plan size has to be constant!  Thanks, guys.
;; So we separated the commands with 2 elements from the rest.  Highlight_set
;; is in both.  WTF.
;;
;; There are many examples here of how the system was intended to use.
;; An inner loop for the put command - along with a string capture.  Yes,
;; each character comes in as an array of one element, containing a string of
;; one character.  That's right.  Oh yeah, put is another variable size
;; command that ignores the ability to use a data array of the right size and
;; makes you guess and keep state.
;;				       
;; Well it does use data arrays just to offend your sensibilities,
;; waste your time and make you wonder what the rest of neovim code looks like,
;; the parts that are not trivial.  For a reboot project with a clean slate,
;; a chance to do something sensible, someone certainly had their head so far
;; up their ass that it came out of the mouth, and went back into the ass,
;; never to be seen again.  The convoluted strings inside arrays inside
;; command arrays - sometimes - is certainly an example of the worst production
;; protocol I've ever seen.
;;
;;
;; And no, it is not to keep UTF characters.  Messagepack supports UTF8
;; characters directly.  It is just purely to torture you - just like 
;; choosing an amazingly compact and largely stateless protocol like
;; like messagepack, and then shitting all over it to make sure that it
;; is no longer compact or stateless.  And making every message a little
;; different from the rest.  A map in one, an array in another, an array of
;; arrays in the third, maybe an array of maps or strings.  All different
;; lengths, some with data separated, some just a part of the top array.
;; Yes, folks.  I mean, really, just install python and load a damn
;; library that convers messagepack to JSON, consing up megabytes of data
;; to be thrown away.  Ugh.
;;

#||
(connect (make-instance 'nvim :plan *plan*) 50 10)
(vin *v*)(! ":r ~/fuck.txt
")
(dotimes (i #x150) (vimbyte *v*))
(vimbyte *v*)
||#

(defparameter *plan-redraw*
  (parse-plan
   '(:LOOP
     %ARR
     (2 :STRINGS
      ("resize"    %ARR 2 %U * :VAR 1  %U * :VAR 2 :FUN (resize 1 2))
      ("scroll"    %I * :VAR 1 :FUN (scroll 1))
      ("update_fg" %ARR 1 %S * :VAR 1 :FUN (update 'fg 1))
      ("update_bg" %ARR 1 %S * :VAR 1 :FUN (update 'bg 1))
      ("update_sp" %ARR 1 %S * :VAR 1 :FUN (update 'sp 1))
      ("clear"     %ARR 0 :FUN (clear))
      ("cursor_goto"   %ARR 2 %U * :VAR 1  %U * :VAR 2 :FUN (cursor-goto 1 2))
      ("eol_clear" %ARR 0 :FUN (eol-clear))
      ("mode_info_set" %ARR 2 %BOOL * :VAR 1 :FUN (mis-cursor-style-enabled 1)
       :GOTO *plan-mis*)
      ("mode_change" %ARR 2 %STR * :STR 1 %U * :VAR 2 :FUN (mode-change 2))
      ;; Yes, variable-size commands featuring idiocy!  Duplicated
      ("put"       :LOOP %ARR 1 %STR * %C * :VAR 1 :FUN (put 1))
      ("highlight_set" %ARR 1 :GOTO *plan-hls*))
     (* :STRINGS
      ("mode_change" :LOOP  %ARR *  %STR * :STR 1 %U * :VAR 2 :FUN (mode-change 2))
      ("put"       :LOOP %ARR 1 %STR * %C * :VAR 2 :FUN (put 2))
      ("highlight_set" %ARR 1 %MAP 0 %ARR 1 :GOTO *plan-hls*)
      ("set_scroll_region" %U * :VAR 1  %U * :VAR 2 %U * :VAR 3 %U * :VAR 4
       :FUN (set-scroll-region 1 2 3 4))))
   ;; mode-info set is an array of maps
))
;; highlight_set       https://neovim.io/doc/user/ui.html#ui-event-highlight_set
;;
;;

;;=========================================================================
;; highlight set, all using a single routine - note 'constants in :FUN...
(defparameter *plan-hls*
  (parse-plan
   '(%MAP
     (0 :FUN (hls nil nil))
     (* :LOOP
      %PAIR 2
      :STRINGS
      ("foreground"  %I * :VAR 1    :FUN (hls 'foreground 1))
      ("background"  %I * :VAR 1    :FUN (hls 'background 1))
      ("special"     %I * :VAR 1    :FUN (hls 'special 1))
      ("reverse"     %BOOL * :VAR 1 :FUN (hls 'reverse 1))
      ("italic"      %BOOL * :VAR 1 :FUN (hls 'italic 1))
      ("bold"        %BOOL * :VAR 1 :FUN (hls 'bold 1))
      ("underline"   %BOOL * :VAR 1 :FUN (hls 'underline 1))
      ("undercurl"   %BOOL * :VAR 1 :FUN (hls 'undercurl 1))))))

;;=========================================================================
;; MIS - mode_set_info command - consists of an array of mode descriptions
;; that must be converted into an array of mode data...
;; Here, the :LOOP after the map binds to a counter that is loaded into
;; :VAR 0 - and passed to the :FUN call for proper placement.
(defparameter *plan-mis*
 (parse-plan
  '(%ARR * :VAR 1 :FUN (mis-size 1)
    :LOOP :INDEX 0
    %MAP * :LOOP 
    %PAIR 2
    :STRINGS
    ("cursor_shape"   :STRINGS
     ("block"      :FUN (mis 0 'cursor_shape 'block))
     ("vertical"   :FUN (mis 0 'cursor_shape 'vertical))
     ("horizontal" :FUN (mis 0 'cursor_shape 'horizontal)))
     
    ("cell_percentage" %U * :VAR 1 :FUN (mis 0 'cell_percentage 1))
    ("blinkwait"       %U * :VAR 1 :FUN (mis 0 'blinkwait 1))
    ("blinkon"         %U * :VAR 1 :FUN (mis 0 'blinkon 1 ))
    ("blinkoff"        %U * :VAR 1 :FUN (mis 0 'blinkoff 1))
    ("hl_id"           %U * :VAR 1 :FUN (mis 0 'hl_id 1))
    ("id_lm"           %U * :VAR 1 :FUN (mis 0 'id_lm 1))
    ("short_name"    %STR * :STR 1 :FUN (mis 0 'short_name 1))
     ("name"          %STR * :STR 1 :FUN (mis 0 'name 1))
     ("mouse_shape"     %U * :VAR 1 :FUN (mis 0 'mouse-shape 1)))))



;;=======================================================
;; hook up to a vim...
(defclass nvim (rpc)
  ((process :accessor process)
   (slurp   :accessor slurp)
   (barf    :accessor barf)
   ;; vim state
   (fg :accessor fg :initform nil)
   (bg :accessor bg :initform nil)
   (sp :accessor sp :initform nil)
   (w :accessor w :initform nil)
   (h :accessor h :initform nil)
   (x :accessor x :initform nil)
   (y :accessor y :initform nil)
   (highlight :accessor highlight :initform nil)
   (minfo :accessor minfo :initform nil)
   (mode :accessor mode :initform nil)
   (buf  :accessor buf :initform nil)
   ;; 
   ))
#||(defmethod print-object ((o nvim) s)
  (print-unreadable-object (o s :type t)
    (format s ":~A"  9)))
||#

;; Response function is not currently used...
(defun response (obj id a b)
  (declare (ignore obj id a b))
  )

;;==============================================================================
;; TODO: are we expected to preserve data?
(defun resize (obj width height)
  (with-slots (buf w h) obj
    (setf w width
	  h height
	  buf (make-array h))
    (loop for i from 0 below height do
	 (setf (aref buf i)
	       (make-array w :element-type 'character
			   :initial-element #\space)))
    buf))
;; TODO: implement
(defun scroll (obj i)
  (declare (ignore obj i))
  (error "SCROLL not implemented"))

;;==============================================================================
;; color updates
(defun update (obj typ value)
  (with-slots (fg bg sp) obj
    (case typ
      (FG (setf fg value ))
      (BG (setf bg value ))
      (SP (setf sp value )))))
;;==============================================================================
;; 
(defun clear  (obj)
  (with-slots (buf w h)obj
    (loop for l across buf do
	 (loop for j from 0 below w do
	      (setf (aref l j) #\space)))))
;;==============================================================================
;;
(defun cursor-goto (obj yy xx)
  (with-slots (x y) obj
    (setf x xx
	  y yy)))
;;==============================================================================
;;
(defun eol-clear (obj)
  (with-slots (buf x y w h) obj
      (loop for i from x below w 
	 with line = (aref buf y) do 
	   (setf (aref line i) #\space))))
;; TODO: move the cursor or not?
;;==============================================================================
;; TODO:
(defun mis-cursor-style-enabled (obj bool)
  (declare (ignore obj bool))
  (warn "MIS-CURSOR-STYLE-ENABLED not implemented")
  )
;;==============================================================================
;;
(defun mis-size (obj size)
  (with-slots (minfo) obj
    (format t "MODE COUNT: ~A~&"size) 

    (setf minfo (make-array size :initial-element nil))))
;;==============================================================================
;; TODO: not quite right
(defun mis (obj index type value)
  (with-slots (minfo) obj
    (push (cons type value) (aref minfo index))))
;;==============================================================================
;;
(defun mode-change (obj index)
  (setf (mode obj) index))
;;==============================================================================
;;
(defun put (obj value)
  (with-slots (x y buf) obj
    (setf (aref  (aref buf y) x) (code-char value))
    (incf x)))
;;==============================================================================
;; TODO
(defun set-scroll-region (obj x y w h)
  (declare (ignore obj x y w h))
  (error "SCROLL REGION"))
;;==============================================================================
;; TODO not quite right
(defun hls (obj type value)
  (with-slots (highlight) obj
    (let ((it (assoc type highlight)))
      (if it (setf (cdr it) value)
	  (push (cons type value) highlight)))))

;;------------------------------------------------------------------------------
;;
;; start command header, optionally requesting a when-done callback (if error
;;
(defun !command (vim command paramcnt )
  (let ((s (barf vim)))
    (!array-header s 4) ;; command array
    (!ints s 0 8) ;;TODO: seq
    (!string s command)
    (!array-header s paramcnt)
    s))

(defun nvim-ui-attach (vim rows cols &optional )
  (let ((s (!command vim "nvim_ui_attach" 3)))
    (!ints s rows cols)
    (!map-header s 0)
    (force-output s)))

(defparameter *v* nil)
(defparameter *in* nil)

(defun connect (vim rows cols)
  (with-slots (process slurp barf) vim
    (setf process (external-program:start
		   "nvim" '("--embed") :input :stream :output :stream)
	  slurp  (external-program:process-output-stream process)
	  barf   (external-program:process-input-stream process))

    (setf (plan vim)  *plan*)
    
    (nvim-ui-attach vim rows cols)

    (setf *v* vim
	  *in* slurp)))
;;==============================================================================
;;
(defun vimbyte (&optional (vim *v*))
  (let ((byte (read-byte (slurp vim) )))
    (format t "~A: ~02X ~A" (total vim) byte (code-char byte)) ;;(if (< byte 128)		(code-char byte)  #\.)
	    
    (decode vim byte)
    vim)
  )
;;(connect (make-instance 'nvim) 50 12)
;;==============================================================================
;; Process all available input!
;;
(defun vin (&optional (vim *v*))
  (prog1
      (loop while (listen (slurp vim))
	 for i from 0 do
	   (decode vim (read-byte (slurp vim)))
	 finally (return  i))
    (print (buf vim)))
  )

(defun ! ( keys &optional (vim *v*))
  (let ((barf (!command vim "nvim_input" 1)))
    (!string barf keys)
    (force-output barf)))
