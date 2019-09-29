;;;; cl-gal.lisp
;;; This project aims to create an abstract syntax tree which can then later be compiled into valid Graphviz dot code

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
  `(node (:name . ,name) (:options . ,(mk-klist attrs))))

(defun nodep (node)
  "Checks if a list is a node"
  (eq 'node (car node)))

(defun edge (arrow targets &rest attrs)
  "Defines an edge. :arrow indicates the arrow style used in Graphviz, either -- or ->.
It is left open to accomodate possible future arrow styles."
  (assert (>= (length targets) 2) (targets)
	  "The list ~a is too short" targets)
  `(edge (:arrow . ,arrow) (:targets . ,targets) (:options . ,(mk-klist attrs))))

(defun -> (targets &rest attrs)
  "Convenience function that creates an edge with a -> type arrow"
  (apply #'edge "->" targets attrs))

(defun -- (targets &rest attrs)
  "Convenience function that creates an edge with a -- type arrow"
  (apply #'edge "--" targets attrs))

(defun edgep (edge)
  "Checks if a list is an edge"
  (eq 'edge (car edge)))

(defun graph (type name &rest body)
  "Constructs a graph"
  `(graph (:type . ,type) (:name . ,name) (:body . ,body)))

(defun graphp (graph)
  (eq 'graph (car graph)))

(defun has-options-p (obj)
  "checks if obj is of any type that supports the :options keyword"
  (or (nodep obj) (edgep obj)))

(defun get-field (key obj)
  "Gets a field from within a structure"
  (let ((pure-alist (cdr obj)))
    (cdr (assoc key pure-alist))))

(defun get-options (obj)
  "returns the :options alist"
  (get-field :options obj))

(defun get-name (obj)
  "returns the :name field"
  (get-field :name obj))

(defun get-arrow (obj)
  "returns the :arrow field"
  (get-field :arrow obj))

(defun get-targets (obj)
  "returns the :targets field"
  (get-field :targets obj))

(defun get-body (obj)
  "returns the :body field"
  (get-field :body obj))

(defun get-type (obj)
  "returns the :type field"
  (get-field :type obj))

(defun pairp (pair)
  "Checks if a cons is a pair"
  (and (consp pair)
       (not (consp (cdr pair)))))

(defun option-pair-to-string (pair)
  "Turns a pair of the form (foo . bar) into a string of the form foo = \"bar\""
  (assert (pairp pair) (pair)
	  "Argument ~a is not a valid pair" pair)
  (destructuring-bind (key . value) pair
    (format nil "~a = ~s" (string-downcase key) value)))

(defun option-alist-to-gv-options (alist)
  (let ((options (mapcar #'option-pair-to-string alist)))
    (format nil "[~{~a~^, ~}]" options)))

(defun node-to-gv-node (node)
  "Turns a node object into a string representing a node in graphviz"
  (assert (nodep node) (node)
	  "The entered structure ~a is not a valid node" node)
  (let ((name (get-name node))
	(options (get-options node)))
    (format nil "~a~@[~a~];" name (option-alist-to-gv-options options))))

(defun edge-to-gv-edge (edge)
  (assert (edgep edge) (edge)
	  "The entered structure ~a is not a valid edge" edge)
  (let* ((arrow (get-arrow edge))
	 (targets (get-targets edge))
	 (options (get-options edge))
	 (format-string (concatenate 'string "~{~a ~^" arrow " ~}~@[~a~];")))
    (format nil format-string targets (option-alist-to-gv-options options))))

(defun graph-to-gv-graph (graph)
  (assert (graphp graph) (graph)
	  "The entered structure ~a is not a valid graph" graph)
  (let ((type (get-type graph))
	(name (get-name graph))
	(components (mapcar #'dispatch-ast (get-body graph))))
    (format nil "~a ~a {~%~{    ~a~%~}}" type name components)))

(defun dispatch-ast (item)
  "identifies the type of list and dispatches the correct converter function"
  (cond
    ((nodep item) (node-to-gv-node item))
    ((edgep item) (edge-to-gv-edge item))
    ((graphp item) (graph-to-gv-graph item))))

(defparameter *example-node-1*
  (node "c" :title "C" :shape "rectangle"))

(defparameter *example-node-2*
  (node "cpp" :title "C++" :shape "rectangle"))

(defparameter *example-edge*
  (edge "->" '("c" "cpp") :arrowhead "vee" :style "dashed"))

(defparameter *example-structure*
  (list
   *example-node-1*
   *example-node-2*
   *example-edge*))
