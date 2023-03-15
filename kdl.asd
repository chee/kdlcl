(defsystem "kdl"
	:author "chee <yay@chee.party>"
	:description "A reader and printer for KDL documents."
	:version "1.0"
	:depends-on ("uiop" "esrap" "parse-number")
	:components ((:file "package")
						(:file "api" :depends-on ("package"))
						(:file "read" :depends-on ("package"))
						(:file "print" :depends-on ("read" "api"))
						(:file "io" :depends-on ("read" "print")))
	:in-order-to ((test-op (test-op "kdl/t"))))

(defsystem "kdl/t"
	:author "chee <yay@chee.party>"
	:description "kdl test system."
	:pathname "t/"
	:depends-on ("kdl")
	:components ((:file "t"))
	:perform (test-op (op c)
					(symbol-call :kdl/t :run-test-cases)))
