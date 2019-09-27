;;;; cl-gal.lisp

(in-package #:cl-gal)

(defun mk-klist (args)
  "takes a plist of data and returns a keyed alist as its result. 
i.e. an alist where the first element is a :keyword"
  (assert (evenp (length args)) (args)
	  "arglist ~a is not even" args)
  (loop :for (k v) :on args :by #'cddr
     :do (check-type k keyword)
     :collect (cons k v)))

(defun node (name &rest attrs)
  "Defines a node"
  `(node (:name ,name) (:options . ,(mk-klist attrs))))

(defun nodep (node)
  "Checks if a list is a node"
  (eq 'node (car node)))

(defun edge (arrow targets &rest attrs)
  "Defines an edge. :arrow indicates the arrow style used in Graphviz, either -- or ->.
It is left open to accomodate possible future arrow styles."
  `(edge (:arrow ,arrow) (:targets . ,targets) (:options . ,(mk-klist attrs))))

(defun edgep (edge)
  "Checks if a list is an edge"
  (eq 'edge (car edge)))

(defun has-options-p (obj)
  "checks if obj is of any type that supports the :options keyword"
  (or (nodep obj) (edgep obj)))

(defun get-options (obj)
  "returns the :options alist"
  (when (has-options-p obj)
    (let ((pure-alist (cdr obj)))
      (cdr (assoc :options pure-alist)))))

(defun get-name (obj)
  "returns the :name field"
  (let ((pure-alist (cdr obj)))
    (cadr (assoc :name pure-alist))))