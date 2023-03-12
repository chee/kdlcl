(defsystem "kdl"
	:author "chee <yay@chee.party>"
	:version "0.0"
	:depends-on ("alexandria" "esrap")
	:components ((:file "kdl"))
	:in-order-to ((test-op (test-op "kdl/t"))))

(defsystem "kdl/t"
	:author "chee <yay@chee.party>"
	:pathname "t/"
	:depends-on ("kdl")
	:components ((:file "t"))
	:perform (test-op (op c)
					(symbol-call :kdl/t :run-test-cases)))
