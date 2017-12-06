;;;; package.lisp

(defpackage #:happy-neovim
  (:nicknames #:hn)
  (:use #:cl)
  
  (:export
   ;; basic atomic types
   %ERR %BOOL %U %S %BYTE %C %SINGLE %DOUBLE
   ;; aggregate tyeps
   %STR %ARR %MAP %EXT %BIN %PAIR
   ;; combination masks
   %ANY %NONE %AGG %I %FLOAT %N
   ;; reader class
   reader 
   decode

   ;; writer
   !int !ints !string-header !string !array-header !map-header

   
   ;; treader
   treader object-handler
   ;; lockstep
   ;;
   rpc plan
   ;; plan
   ;;
   parse-plan
   ))



