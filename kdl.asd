(defsystem "kdl"
	:author "chee <yay@chee.party>"
	:version "1.0"
	:depends-on ("uiop" "esrap" "parse-number")
	:components ((:file "package")
						(:file "api")
						(:file "read")
						(:file "print")
						(:file "io"))
	:in-order-to ((test-op (test-op "kdl/t"))))

(defsystem "kdl/t"
	:author "chee <yay@chee.party>"
	:pathname "t/"
	:depends-on ("kdl")
	:components ((:file "t"))
	:perform (test-op (op c)
					(symbol-call :kdl/t :run-test-cases)))
