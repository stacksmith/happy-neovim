;;
;;
(asdf:defsystem #:happy-neovim
  :description "captive neovim with a state-machine messagepack"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD 3-clause license"
  :serial t
  :depends-on (:external-program)
  :components ((:file "package")
	       (:file "reader")
	       (:file "writer")
	       (:file "support")
	       (:file "plan")
	       (:file "rpc")
	       (:file "captive-neovim")
	       ))

