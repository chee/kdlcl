(defpackage :kdl
  (:use
    :common-lisp
    :esrap)
  (:import-from :parse-number :parse-number)
  (:export
    :read-document
    :from-string
    :from-file
    :from-stream
    :to-string
    :to-file
    :to-stream

    :value
    :value-type

    :property-name
    :property-value
    :property-type

    :node-name
    :node-type
    :node-properties
    :node-children
    :node-child
    :node-property))

(defpackage :kdl-user
  (:use :common-lisp :kdl))
