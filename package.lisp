;;;; package.lisp

(defpackage #:cl-gal
  (:use #:cl)
  (:export :node
	   :nodep
	   :node-options
	   :edge
	   :edgep
	   :edge-options
	   :->
	   :--
	   :graph
	   :graphp))
